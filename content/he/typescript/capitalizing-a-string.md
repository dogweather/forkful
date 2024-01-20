---
title:                "הפיכת מחרוזת לאותיות ראשיות"
html_title:           "TypeScript: הפיכת מחרוזת לאותיות ראשיות"
simple_title:         "הפיכת מחרוזת לאותיות ראשיות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
דילוג לאות הראשונה במחרוזת משרת להיכנס אל מצב כתוב. זה נחוץ למגוון של מצבים, אחת השימושים הנפוץ ביותר היא לקלוט שמות.

## כיצד ל:
נבנה מחרוזת עם האות הראשונה לא באותיות ראשיות, ואז נשנה אותה. 

```TypeScript
let str = 'hello world';
str = str.charAt(0).toUpperCase() + str.slice(1);  
console.log(str);  // Outputs: 'Hello world'
```

## הצצה מעמיקה
אם נהסטוריה של השפות התכנות משחקת כל שום דבר, הרעיון להפוך את האות הראשונה של מחרוזת לאותיות ראשיות הוא די חדשני. JavaScript, השפה שעליה TypeScript מתבססת, לא כללה את הפונקציה הזו לתחילה, כך שהפכה לדרך נפוצה להראות את היכולת שלך לפתור בעיות.
 
תחליף מהיר אך פחות אלגנטי היה להפוך את כל המחרוזת לאותיות ראשיות:

```TypeScript
let str = 'hello world';
str = str.toUpperCase();  
console.log(str);  // Outputs: 'HELLO WORLD'
```

אם אתה רוצה להפוך רק את האות הראשונה של כל מילה במחרוד לאותיות גדולות, אתה יכול להשתמש בשיטה זו:

```TypeScript
let str = 'hello world';
str = str.toLowerCase().split(' ').map(word => word.charAt(0).toUpperCase() + word.substring(1)).join(' ');
console.log(str);  // Outputs: 'Hello World'
```

## ראה גם:
1. [TypeScript ב-Vscode](https://code.visualstudio.com/docs/languages/typescript)
2. [המדריך הרשמי ל-TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html)