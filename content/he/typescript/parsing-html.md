---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## ?מה ולמה
פרסינג HTML הוא התהליך שבו אנחנו "קוראים" את קוד HTML ומהפכים אותו למבנה שניתן להתמנע ממנו בקוד. בהקשר של פיתוח, זה מאפשר לנו לתמנע ולשלוט על קוד HTML באופן דינמי.

## מדריך: 
אז איך אנחנו עושים את זה בפועל? ניתן להשתמש בספרייה בNode.js בשם htmlparser2:
```TypeScript
import * as htmlparser from "htmlparser2";

const rawHTML = "<html><body>ברוך הבא למעלה!</body></html>";

const parser = new htmlparser.Parser({});
parser.write(rawHTML);
parser.end();

console.log(rawHTML);
```

אתה צפוי לראות בתוצאה את המחרוזת:
```
"<html><body>ברוך הבא למעלה!</body></html>"
```

## צלילה עמוקה:
בעבר, פרסינג HTML היה די מסובך. ב[[שם|https://en.wikipedia.org/wiki/HTML_parsing]] הוסבר על HTML parsing ואיך התפתח במשך השנים. היום, יש הרבה ספריות חיצוניות אשר מתייחסות לגרסאות שונות של HTML ומאפשרות כתיבת קוד בצורה יותר ידידותית ואינטואיטיבית.

## עיון נוסף:
1. [[שם|https://developer.mozilla.org/he/docs/Web/HTML]] - מדריך מלא לHTML מ-Mozilla.
2. [[שם|https://www.w3schools.com/html/]] - מדריך לHTML מ-W3Schools.
3. [[שם|https://htmldom.dev/]] - מדריך למניפולציות עם DOM בJavaScript.