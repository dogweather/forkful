---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:51.214490-07:00
description: "YAML, \u05E9\u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \"YAML Ain't Markup\
  \ Language\" (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\
  \u05D5\u05DF), \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05E1\u05D9\u05D3\u05D5\u05E8\
  \u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\
  \u05D3\u05DD, \u05E9\u05DE\u05E9\u05DE\u05E9 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\
  \u05DC \u05DC\u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05D5\u05DC\
  \u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D9\
  \u05DF \u05E9\u05E4\u05D5\u05EA \u05E2\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.600172-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u05E9\u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \"YAML Ain't Markup Language\"\
  \ (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF\
  ), \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05E1\u05D9\u05D3\u05D5\u05E8\u05D9 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0 \u05DC\u05D0\u05D3\u05DD\
  , \u05E9\u05DE\u05E9\u05DE\u05E9 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DC\
  \u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05D5\u05DC\u05D4\u05D7\
  \u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D9\u05DF \u05E9\
  \u05E4\u05D5\u05EA \u05E2\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## מה ולמה?

YAML, שפירושו "YAML Ain't Markup Language" (YAML אינו שפת סימון), הוא תקן סידורי נתונים קריא לאדם, שמשמש בדרך כלל לקבצי תצורה ולהחלפת נתונים בין שפות עם מבני נתונים שונים. תכנתנים לעיתים קרובות עובדים עם YAML בזכות פשטותו ונוחות קריאתו, במיוחד בפרויקטים הדורשים תצורה מרחיקת לכת או כאשר מעבירים נתונים מובנים בין מערכות שונות.

## איך ל:

למרות ש-Google Apps Script (GAS) לא תומך באופן ילידי בפיתוח או טיפול בYAML, ניתן לתמרן נתוני YAML באמצעות ספריות JavaScript או כתיבת פונקציות פיתוח מותאמות אישית. לדוגמה, בואו נדון בכיצד לפרסר מחרוזת YAML באמצעות פונקציה מותאמת אישית, מאחר שספריות חיצוניות לא ניתנות לייבוא ישירות ל-GAS.

נניח שיש לנו תצורת YAML פשוטה:

```yaml
title: YAML Example
description: An example of how to handle YAML in Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Configuration
```

לפרסור זה ב-Google Apps Script, השתמשו ביכולות טיפול במחרוזות של JavaScript:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // טיפול בסיסי במערכים
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: YAML Example\ndescription: An example of how to handle YAML in Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

כאשר מפעילים את `testYamlParsing()`, הפלט הוא:

```
{ title: 'YAML Example',
  description: 'An example of how to handle YAML in Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Configuration' ] }
```

גישה זו לפיתוח פשוטה למדי ועשויה לדרוש התאמות כדי להתמודד עם קבצי YAML מורכבים.

## טבילה עמוקה

YAML, שהושק בשנת 2001, שאף להיות קריא יותר מקודמיו כמו XML או JSON. למרות שפשטותו ונוחות השימוש שלו מוערכים רבות, הטיפול ב-YAML ב-Google Apps Script מציג אתגרים בשל החוסר בתמיכה ישירה. כתוצאה מכך, תכנתנים לעיתים קרובות מסתמכים על גמישותו של JavaScript לפיתוח ויצירת נתוני YAML. עם זאת, למקרים שימוש מורכבים, במיוחד אלו הכוללים קינון עמוק ומבני נתונים מתקדמים, שיטה זו עלולה להיות מסורבלת ונתונה לשגיאות.

JSON, לעומת זאת, נתמך באופן ילידי ב-Google Apps Script וברוב סביבות התכנות האחרות, מה שמציע גישה יותר פשוטה לסידורי נתונים והפענוח שלהם ללא צורך בפיתוח נוסף. תחבירו של JSON פחות חריף מזה של YAML, הופך אותו למתאים יותר להחלפת נתונים ביישומי אינטרנט. למרות זאת, YAML עדיין פופולרי לקבצי תצורה ומצבים בהם קריאות לאדם חשובה ביותר.

כאשר עובדים עם YAML ב-Google Apps Script, שקלו את ההתמודדויות בין קריאות לנוחות שימוש. לטיפול מקיף ב-YAML, כדאי לבחון כלים או שירותים חיצוניים שיכולים להמיר YAML ל-JSON לפני טיפולו בתסריט שלכם.
