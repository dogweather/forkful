---
title:                "התחלת פרויקט חדש"
aliases: - /he/typescript/starting-a-new-project.md
date:                  2024-01-20T18:05:40.732693-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
התחלת פרויקט חדש בפיתוח תוכנה היא כמו לקפוץ לבריכה: הרעיון הוא לקחת רעיון ולהפוך אותו לקוד פעיל. תכנתים עושים זאת כדי ליצור מוצרים חדשים, לפתור בעיות או לחקור טכנולוגיות חדשות.

## איך לעשות:
```TypeScript
// התקנה של TypeScript בעזרת npm - Node.js Package Manager
npm install -g typescript

// איתחול פרויקט חדש עם קובץ tsconfig.json
tsc --init

// יצירת קובץ TypeScript הראשון שלך, התחלה.ts
echo 'console.log("שלום עולם!");' > התחלה.ts

// קמפילציה לJavaScript והרצה
tsc התחלה.ts
node התחלה.js
```
פלט דוגמא:
```
שלום עולם!
```

## צלילה עמוקה:
TypeScript הוא לשון תכנות שפותחה על ידי מיקרוסופט ב-2012. זו שפה המוסיפה טיפוסים בזמן קומפילציה ל-JavaScript, הופכת את הקוד ליותר בטוח וניתן לתחזוקה. ישנן שפות חלופיות כמו Dart או Reason, אך TypeScript נהנית מפופולריות רבה בזכות התמיכה שלה בסטנדרטים חדשים של ESNext והאינטגרציה החלקה עם סביבות JavaScript קיימות. בתוך הקובץ `tsconfig.json` ניתן להגדיר הגדרות שונות לקומפילציה כמו גרסת JavaScript יעד, מודולים, ואפשרויות קומפילציה אופטימיזציות.

## ראה גם:
- [תיעוד רשמי של TypeScript](https://www.typescriptlang.org/docs/)
- [חינוך של TypeScript בעברית](https://www.youtube.com/playlist?list=PLI5t0u6ye3FGy_hL_6-bQkG6ZBvQeqFt3)
- [TypeScript Deep Dive](https://basarat.gitbook.io/typescript/), מדריך מעמיק מאוד על TypeScript
