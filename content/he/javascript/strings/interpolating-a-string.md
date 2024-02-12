---
title:                "שרבוב מחרוזת"
aliases:
- /he/javascript/interpolating-a-string/
date:                  2024-01-20T17:51:14.654867-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מילוי מחרוזת ב-JavaScript הוא כאשר אנו מזריקים ערכים דינמיים לתוך מחרוזת טקסט. זה מאפשר כתיבה יעילה וקריאה יותר של קוד על ידי שילוב נתונים עם טקסט בצורה נקייה ואלגנטית.

## איך לעשות:
ב-JavaScript, אנחנו משתמשים בתבניות מחרוזת (string templates) כדי לבצע מילוי. הנה דוגמה:

```Javascript
let firstName = 'ישראל';
let lastName = 'ישראלי';
let greeting = `שלום, ${firstName} ${lastName}!`;

console.log(greeting); // פלט: שלום, ישראל ישראלי!
```
וכך זה נראה עם פונקציות:

```Javascript
function getWelcomeMessage(name) {
  return `ברוך הבא, ${name}!`;
}

console.log(getWelcomeMessage('דני')); // פלט: ברוך הבא, דני!
```

## צלילה לעומק:
מילוי מחרוזות שייך לעולם התיכנות מדורות ראשונים. ב-JavaScript, עד ES5, למתכנתים הוצעו אפשרויות כמו שילוב עם אופרטור החיבור (`+`). ES6 הביא את הסינטקס עם סימני גרש הנקראים Template Literals, ושינה את המשחק. זה לא רק פשט את הקוד, אלא גם הפחית את הטעויות והבלבול.

דוגמאות לחלופות: בעבר, פונקציות כמו `sprintf()` או פתרונות של תוספות שלדים (frameworks) עשו את העבודה. כיום, פונקציות כמו `replace()` או ספריות כמו Lodash יכולות להציע גמישות רבה יותר. 

פרטים טכניים כוללים הבנת התחביר כולל השימוש ב`${}` והיכולת לשלב ביטויים ישירות לתוך המחרוזת, לחשב אותם במהלך הריצה, במיוחד עם ביטויים מסובכים או קריאות לפונקציות.

## ראו גם:
- [MDN Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals) - מדריך רשמי בנושא תבניות מחרוזת.
- [MDN String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) - כל המידע על אובייקט String ב-JavaScript.
