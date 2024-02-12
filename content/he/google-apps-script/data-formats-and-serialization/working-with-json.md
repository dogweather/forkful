---
title:                "עבודה עם JSON"
aliases:
- /he/google-apps-script/working-with-json/
date:                  2024-02-01T22:05:57.670761-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

JSON, או JavaScript Object Notation, הוא פורמט קליל לאחסון והעברת נתונים, אידיאלי לתקשורת בין שרת ללקוח ולקובצי תצורה. מתכנתים מנצלים אותו ב-Google Apps Script לשם חילופי נתונים חלקים בין שירותי גוגל (כמו Sheets, Docs, Drive) לבין מקורות חיצוניים, בשל מבנהו הקריא לאדם והשילוב הקל שלו בסביבות מבוססות JavaScript.

## איך לעשות:

ב-Google Apps Script, התעסקות עם JSON היא תהליך ישיר למדי, במידה רבה בזכות התמיכה הטבעית ש-JavaScript מספקת לפענוח JSON ולהפיכתו למחרוזת. להלן כמה פעולות נפוצות:

**1. פענוח JSON**: נניח שאנו מקבלים מחרוזת JSON משירות ובי; המרתה לאובייקט JavaScript היא חיונית לשם עיבוד נתונים.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // פלט: Sample Project
```

**2. הפיכת אובייקטים של JavaScript למחרוזת JSON**: לחלופין, המרת אובייקט JavaScript למחרוזת JSON מועילה כאשר אנו צריכים לשלוח נתונים מ-Google Apps Script לשירות חיצוני.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // פלט: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. עבודה עם נתונים מורכבים**:
עבור מבני נתונים יותר מורכבים, כמו מערכים של אובייקטים, התהליך נשאר אותו הדבר, מה שמדגים את גמישותו של JSON לייצוג נתונים.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // פלט: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## צלילה עמוקה

אי אפשר להפריז בחשיבותו של JSON ביישומי הרשת המודרניים, הנבסס על פשטותו ועל אופן השילוב החלק שלו עם JavaScript, שפת הרשת. תכנונו, הנעור בהשראת מילולים של אובייקטים ב-JavaScript, אם כי בצורה נוקשה יותר, מקל על קבלתו לשימוש. בתחילת שנות ה-2000, JSON זכה לפופולריות כאלטרנטיבה ל-XML עבור יישומי רשת המופעלים ב-AJAX, והציע פורמט חילופי נתונים קליל ופחות מלומד ממנו. בהתחשב בשילובה העמוק של Google Apps Script עם מגוון שירותי Google ושירותים חיצוניים, JSON משמש כפורמט מרכזי למבנה, העברה ועיבוד נתונים ברחבי הפלטפורמות האלו.

למרות ש-JJSON מוביל ליישומי רשת, ישנם פורמטים נתונים אלטרנטיביים כמו YAML עבור קבצי תצורה או Protobuf עבור סריאליזציה בינארית יעילה יותר בסביבות ביצועים גבוהות. עם זאת, האיזון של JSON בין קריאות, נוחות שימוש ותמיכה רחבה בשפות תכנות ובכלים הופכים אותו לבחירה ברירת המחדל של רבים מהמתכנתים הצוללים לעולם של Google Apps Script ומעבר לכך.
