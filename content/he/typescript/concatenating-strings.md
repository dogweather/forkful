---
title:                "צירוף מחרוזות"
html_title:           "TypeScript: צירוף מחרוזות"
simple_title:         "צירוף מחרוזות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?

קונקטיזציה של מחרוזות היא פעולה שמשמשת לשלב יחד יותר מקבצי טקסט אחד בכדי ליצור מחרוזת אחת גדולה יותר. תהליך זה משמש ליצירת מחרוזות פתרון רב-תכליתיות, לדוגמה ליצירת רשימת תווים או לבניית הודעות למשתמש.

## איך לבצע:

כאשר תרצו לקונקטיז מחרוזות ב- TypeScript, ישנם שלושה אפשרויות: תוך כדי ההכרזה על משתנה חדש, באמצעות פעולת השמה, או באמצעות תוכנית פונקציה. להלן כמה דוגמאות של שימוש בפעולה זו:

```TypeScript
// הוספת מחרוזת נוספת לסוף מחרוזת נמצאת במשתנה
let sentence: string = "Hello, ";
sentence += "world!";

console.log(sentence); // Output: "Hello, world!"

// קונקטיזציה באמצעות פעולת השמה
let firstWord: string = "Hello ";
let secondWord: string = "world!";
let newSentence = firstWord + secondWord;

console.log(newSentence); // Output: "Hello world!"

// שימוש בפונקציה לקונקטיזציה
function combineStrings(name: string, age: number): string {
  return "My name is " + name + " and I am " + age + " years old.";
}

console.log(combineStrings("John", 25)); // Output: "My name is John and I am 25 years old."
```

## חפירה עמוקה:

הקונקטיזציה של מחרוזות היא טכניקה פשוטה שמשמשת בשפות תכנות שונות כדי להציג מחרוזת או רשימה של תווים. בעבר, השיטה המועט יותר פשוטה היא באמצעות שרשרת תווים, אשר הכוונה אליה היא פשוט להוסיף טקסט או מחרוזת לסוף מחרוזת אחרת. אך כיום, קיימות פתרונות מתקדמים יותר כגון String Builder אשר מאפשרים יישום נוסף ואפשרויות יותר גמישות.

## ראו גם:

- [קונקטיזציה של מחרוזות ב-TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-concatenation)
- [String Builder ב-TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#stringbuilder)