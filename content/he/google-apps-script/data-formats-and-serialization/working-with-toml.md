---
title:                "עובדים עם TOML"
date:                  2024-02-01T22:06:46.090654-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

TOML, שמסמל את Tom's Obvious, Minimal Language, הוא פורמט קובץ תצורה שקל לקרוא בזכות הסמנטיקה הברורה שלו. מתכנתים לעיתים קרובות משתמשים בו לקבצי תצורה ביישומים מכיוון שהוא ישיר וקריא לאדם, דבר שהופך את ניהול הגדרות ותצורות של יישום לחלק בלתי נפרד בסביבות שונות.

## איך לעשות:

כיוון ש-Google Apps Script הוא בעצם JavaScript עם גישה לערכת האפליקציות של גוגל, עבודה עם TOML ישירות בתוך Google Apps Script דורשת קצת תחכום. Google Apps Script לא תומך באופן ילידי בפריסת TOML, אך אפשר לנצל ספריות JavaScript או לכתוב פרסר פשוט לצרכים בסיסיים.

בואו ננתח רצף תצורת TOML פשוט כדוגמה:

```javascript
// רצף TOML
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// פונקציה של פרסר TOML ל-JSON פשוט
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // סעיף חדש
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // שימוש ב-eval לפשטות; נא להיזהר בקוד ייצור
      currentSection[key] = value;
    }
  });
  return result;
}

// בדיקת הפרסר
var configObject = parseTOML(tomlString);
console.log(configObject);

```

הפלט מה-`console.log`יתראה כמו אובייקט JSON, דבר שהופך את הגישה לפרמטרים של התצורה בתוך Google Apps Script לקלה יותר:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## טבילה עמוקה

TOML נוצר על ידי טום פרסטון-ורנר, אחד ממייסדי GitHub, במטרה להיות ידידותי יותר לאדם מאשר JSON לקבצי תצורה תוך שמירה על היכולת להיפרש ללא דו משמעות. הוא שואף להיות פשוט ככל האפשר, מטרה שמתאימה יפה לאתוס של פרויקטים רבים ששואפים לפשטות ולקריאות במסדות הקוד שלהם.

בהקשר של Google Apps Script, שימוש ב-TOML יכול לגרום לעלות בהתחייבות, בהתחשב בחוסר התמיכה הישירה והצורך לפרס אותו ידנית או דרך ספריות של צד שלישי. לפרויקטים קטנים יותר או אלה שאינם משולבים באופן עמוק באקוסיסטם של גוגל, חלופות כמו JSON או אפילו מבני זוגות מפתח-ערך פשוטות בתכונות הסקריפט עשויות להספיק ולהיות פשוטות יותר ליישום. עם זאת, עבור יישומים שמעדיפים קבצי תצורה שנוחים לאדם וכבר מחויבים ל-TOML, הטמעת פריסת TOML דרך סקריפטים מותאמים אישית מוסיפה שכבה שימושית של גמישות וקלות תחזוקה מבלי לסטות מהפרדיגמות המועדפות של התצורה.
