---
title:                "Javascript: המרת תאריך למחרוזת"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

המרה של תאריך למחרוזת היא דבר חשוב לעשות בתכנות ב-Javascript. תאריך הוא ערך יסודי וכדאי להציג אותו בצורה ייצוגית התקינה כדי לשמור על קלות הקריאה וההבנה של קוד.

## איך לעשות

הנה כמה דוגמאות שמציגות איך להמיר תאריך למחרוזת ב-Javascript:

```Javascript
// המרת תאריך לתאריך יצירת מחרוזת עם תאריך מלא, חודש ויום
var date = new Date(2021, 7, 1);
var stringDate = date.toLocaleDateString();
// תוצאה: 1/8/2021

// המרת תאריך לקובץ מצורף עם שם ימי השבוע וחודשים בעברית
var date = new Date(2021, 7, 1);
var options = {weekday: 'long', month: 'long', year: 'numeric'};
var stringDate = date.toLocaleDateString('he-IL', options);
// תוצאה: יום רביעי 1 אוגוסט 2021
```

ניתן גם להשתמש בפונקציה מובנית שנמצאת בתוך האובייקט Date עבור מחרוזות תאריך מותאמות למפתח אופציונלי:

```Javascript
// המרת תאריך למחרוזת בתבנית לפי ISO 8601
var date = new Date(2021, 7, 1);
var stringDate = date.toISOString();
// תוצאה: 2021-08-01T00:00:00.000Z
```

בחירת אופציונלים שונים בפונקציות מובנות יכול להפוך את התאריך למחרוזת לכסף נקי בתוך הטקסט.

## Deep Dive

בנוסף לדוגמאות המצורפות, כדאי לדעת כמה פרטים נוספים על התהליך של המרה של תאריך למחרוזת:

- פונצקית toString() הממירה את התאריך למחרוזת בתבנית מותאמת לאתר את תבנית התאריך.
- במספר דפדפנים, פונקציות כמו getDate(), getFullYear() ו getHours() יחזירו ערכים משממים לפונקציית toLocaleString().
- כדי להציג את התאריך בפורמט אחר, ניתן להחליף את ה- return בפ