---
title:    "TypeScript: המרת תאריך למחרוזת"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה
מתי להמיר תאריך למחרוזת מיועד לכתובת קליטה בלבד ולאייבוא ונתונים בפלטים שונים.

## כיצד לעשות זאת
המרת תאריך למחרוזת היא דבר נפוץ בתשתית פיתוח תוכנה. זה כפולבלטם להמיר תאריך למחרוזת בצורה נכונה כדי שתוכל הקלט והפלט להיות במבנה חסכוני ונוח לטיפול בו.

הנה דוגמה לכיצד להמיר תאריך למחרוזת באמצעות פונקציית `toLocaleDateString` בסגנון TypeScript:

```TypeScript
// יצירת אובייקט Date
const date = new Date();

// המרת התאריך למחרוזת עם פורמט יום/חודש/שנה
const dateString = date.toLocaleDateString("he-IL");
console.log(dateString); // יום/חודש/שנה
```

ניתן גם להגדיר פרמטרים נוספים לפונקציית `toLocaleDateString` כדי לקבל פלט מדויק יותר. למשל, הנה דוגמה שמשתמשת באזור זמן ופורמט יום/חודש/שנה בנוסף לשפת `he-IL`:

```TypeScript
// יצירת אובייקט Date
const date = new Date();

// הגדרת אזור זמן
const options = { timeZone: "Asia/Jerusalem" };

// המרת התאריך למחרוזת עם פורמט יום/חודש/שנה ואזור זמן
const dateString = date.toLocaleDateString("he-IL", options);
console.log(dateString); // יום/חודש/שנה, 09:00
```

בכל פעם שאתם משתמשים בפונקציית `toLocaleDateString` תהיה ניתן להגדיר פרמטרים שונים תלוי כלכלה להתאמה לצרכים שלכם.

## נכנס לפרטים
כעת שאנו יודעים כיצד להמיר תאריך למחרוזת באמצעות קוד TypeScript, בואו נתחיל להתעמק בפרמטרים של `toLocaleDateString`. אחד הפרמטרים המשתנים ביותר הוא השפה, שכפה פורמטים שונים על הפלט. לדוגמה, שפות שונ