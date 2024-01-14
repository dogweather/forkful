---
title:    "TypeScript: כתיבה לפלט שגיאה סטנדרטית"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## למה

למה לכתוב לפלט שגיאות סטנדרטי?

לכתוב לפלט שגיאות סטנדרטי נחשב כללי ונפוץ בתחום התכנות. זה מאפשר למתכנתים לקלוט ולטפל בשגיאות שעלו בפעולת התכנות באופן יעיל ומובן. כמו כן, הצגת השגיאות במקום ברור ומדויק עוזרת לפתורן במהירות ולמנוע שגיאות נוספות בעתיד.

## כיצד לבצע

תצורת למקור השגיאות סטנדרטי בקוד טייפסקריפט נעשית באמצעות הפונקציה console.error(). הנה דוגמא פשוטה:

```typescript
let num1: number = 6;
let num2: number = 0;
if (num2 === 0) {
  console.error("Cannot divide by zero!");
} else {
  let result: number = num1 / num2;
  console.log(result);
}
```

הפלט יהיה:

```
Cannot divide by zero!
```

כדי להדפיס תאור מדויק יותר של השגיאה, ניתן להעביר את ערכי המשתנים הרלוונטיים כארגומנטים לפונקציה, כך שההודעה תציג את ערכם הנוכחי:

```typescript
let num1: number = 6;
let num2: number = 0;
if (num2 === 0) {
  console.error(`Cannot divide ${num1} by ${num2}!`);
} else {
  let result: number = num1 / num2;
  console.log(result);
}
```

הפלט יהיה:

```
Cannot divide 6 by 0!
```

## לטלטול עומק

כדי להשתמש בפונקציה console.error() בצורה מתקדמת יותר, ניתן להפעיל אותה רק במצבי שגיאה מסוימים תוך שימוש במנגנוני JavaScript מתקדמים כמו try-catch. זה מאפשר לנו להתמודד עם שגיאות ספציפיות ולהציג את הודעות השגיאה הרלוונטיות.

## ראה גם

- [מדריך לשגיאות סטנדרטיות בקוד טייפסקריפט](https://typescriptlang.org/docs/handbook/errors.html)
- [הסבר על פונקציות מתקדמות בקוד טייפס