---
date: 2024-01-26 04:36:44.920601-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:38.957806-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך לעשות:
```TypeScript
import { parseString } from 'xml2js';

// XML לדוגמה
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>אל תשכח את הפגישה!</body>
             </note>`;

// ניתוח XML ל-JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// בהנחה שהניתוח היה מוצלח, התוצאה תיראה כך:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['אל תשכח את הפגישה!'] } 
}
```

## צלילה עמוקה
XML, או שפת הסימון הנרחבת, הייתה קיימת מאז שנות ה-90 המאוחרות. האופי התיאורי העצמי והפורמט הקריא לאדם שלה הפכו אותה ללהיט מוקדם עבור שימושים שונים כמו הזנות RSS, ניהול תצורה, ואף פורמטים של מסמכי משרד כמו Microsoft Office Open XML. אך, היא מסורבלת לעומת JSON, והמגמה השתנתה. JSON זכתה לתשומת הלב ב-APIs מבוססי אינטרנט בזכות המשקל הקל יותר והתאימות הטבעית עם JavaScript.

עם זאת, XML לא מתה. היא נמצאת בשימוש במערכות ארגוניות גדולות ועבור תקנות של מסמכים שלא הועברו ל-JSON. כלים כמו `xml2js` עבור TypeScript או `lxml` ב-Python מוכיחים שיש צורך מתמשך בעיבוד XML בתכנות.

TypeScript אינו תומך כברירת מחדל ב-XML כמו שהוא תומך ב-JSON. במקום זאת, אתה עובד עם ספריות. `xml2js` הוא דוגמה לכך. הוא ממיר XML ל-JSON, מה שמקל על מומחי JavaScript לעבוד עם הנתונים.

## ראה גם
- [MDN Web Docs על XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [חבילת npm של xml2js](https://www.npmjs.com/package/xml2js)
- [מדריך XML של W3Schools](https://www.w3schools.com/xml/)
