import Parser from "tree-sitter";
import Beancount from "tree-sitter-beancount";

import * as b from "./beancount";

const parser = new Parser() as b.Parser;
parser.setLanguage(Beancount as any);

const sourceCode = `
2023-01-01 * "Opening Balance"
   Assets:Checking:Bank1    1000.00 USD
   Equity:Opening-Balances

2023-01-02 * "Coffee" #food
   Expenses:Food:Coffee       4.50 USD
   Assets:Checking:Bank1     -4.50 USD
`;

const tree = parser.parse(sourceCode);
const file = tree.rootNode as b.FileNode;

for (const x of file.children) {
   switch (x.type) {
   case "transaction":
      console.group(`Transaction on ${x.dateNode.text}`)
      for (const posting of x.children.filter(x => x.type === "posting")) {
         let postingStr = `Posting to ${posting.accountNode.text}`;
         if (posting.amountNode) {
            postingStr += ` for amount ${posting.amountNode.text}`;
         }
         console.log(postingStr);
      }
      console.groupEnd();
      break;
   }
}
