---
title:                "TypeScript: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

בדיקת תאריכים היא חלק חשוב מהתכנות בכל שפת תכנות. בדיקת תאריכים מאפשרת לנו לבדוק אם תאריך אחד הוא קודם או מאוחר יותר מתאריך אחר, וכן לחשב את ההפרש בין שני תאריכים. בפוסט הזה, נלמד כיצד להשתמש בטיפים וכלים נוחים ב-TypeScript לביצוע בדיקות תאריכים.

## איך לעשות זאת

### בדיקת האם תאריך אחד הוא קודם או מאוחר יותר מתאריך אחר

כדי לבדוק אם תאריך אחד הוא קודם או מאוחר יותר מתאריך אחר, נשתמש בפונקציות המובנות ב-TypeScript לעיבוד תאריכים ולהשוואתם. הנה דוגמה של כיצד ניתן להשתמש בפונקציות כדי לבדוק את הסדר של שני תאריכים:

```TypeScript
const date1: Date = new Date(2021, 10, 17);
const date2: Date = new Date(2021, 11, 4);

if (date1.getTime() < date2.getTime()) {
  console.log("date1 is before date2")
} else if (date1.getTime() > date2.getTime()) {
  console.log("date1 is after date2")
} else {
  console.log("date1 is equal to date2")
}
```

פלט משלוח:

```typescript
date1 is before date2
```

בדוגמה זו, אנו יוצרים שני אובייקטים של תאריך, ואז משווים אותם באמצעות הפונקציה getTime המשווה בין תאריך לתאריך באמצעות ערך התאריך המספרי שלהם. מכיוון ש-date1 נמצא לפני-date2 במקרה זה, הפונקציה getTime תחזיר מספר שלילי ולכן לוג הכנסת את ההודעה כי תאריך 1 הוא "לפני" תאריך 2.

### חישוב הפרש בין שני תאריכים

כדי לחשב את ההפרש בין שני תאריכים, נשתמש בפונקציות המובנות לעיבוד תאריכים של TypeScript ונחשב את ההפרש בין שני תאריכים באמצעות הפונקציה getMilliseconds