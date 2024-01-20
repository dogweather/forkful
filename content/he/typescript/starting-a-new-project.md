---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה זה & למה?: 
התחלת פרויקט חדש היא התהליך בו אנו יוצרים את הבסיס להוספת פונקציונליות לקוד שלנו. התחלת פרויקט מאפשרת למתכנתים ליצור כלים חדשים, לפתור בעיות, או להוסיף שיפורים למערכות קיימות.

## איך לעשות: 
הינה דוגמת קוד איך להתחיל פרויקט TypeScript חדש באמצעות npm.

```TypeScript
// הוראות שורת הפקודה:
npm init -y // יוצר package.json 
npm install typescript ts-node --save-dev // מתקין TypeScript
npx tsc --init // יוצר tsconfig.json
```

שימו לב שהפקודה `npx tsc --init` מייצרת קובץ הגדרות בשם `tsconfig.json`, שמגדיר את הגדרות TypeScript.

## צלילה עמוקה: 

1. **ההקשר ההיסטורי**: TypeScript הוא שפת תכנות שנוצרה על ידי Microsoft מבית ב-2012. היא מבוססת על JavaScript, אך מוסיפה טיפוסים מפורשים ותכנות מאובטח.
2. **אלטרנטיבות**: למרות ש-TypeScript מאוד פופולרי, ישנן שפות כמו Flow של Facebook שיכולות להיות אלטרנטיבות ל-TypeScript.
3. **פרטי המימוש**: בפרויקט TypeScript, הקובץ `tsconfig.json` הוא המקום שבו אנחנו מגדירים את הגדרות הפרויקט שלנו – כמו הכוונת אופציות ההדפסה, היכן למצוא קבצים מקוריים, ואיפה לשים קבצי הפלט.

## ראה גם:

* [תיעוד TypeScript](https://www.typescriptlang.org/docs/)
* [דרכים שונות להתחיל פרויקט TypeScript](https://www.freecodecamp.org/news/how-to-start-a-typescript-project/)
* [Flow - שפת תכנות אלטרנטיבית](https://flow.org/)