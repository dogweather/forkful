---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:15.037852-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\
  \u05E1\u05D5\u05D9\u05DE\u05EA \u05D4\u05D9\u05D0 \u05D8\u05DB\u05E0\u05D9\u05E7\
  \u05D4 \u05D4\u05DE\u05E9\u05DE\u05E9\u05EA \u05DC\u05E0\u05D9\u05E7\u05D5\u05D9\
  \ \u05D0\u05D5 \u05E2\u05D9\u05E6\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA. \u05D1\u05D4\u05E7\u05E9\u05E8 \u05E9\
  \u05DC Google Apps Script, \u05E9\u05DE\u05DE\u05E9\u05E7 \u05D1\u05D0\u05D5\u05E4\
  \u05DF \u05E0\u05E8\u05D7\u05D1 \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\
  \ Google \u05DB\u05DE\u05D5\u2026"
lastmod: '2024-02-25T18:49:36.849284-07:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\u05E1\
  \u05D5\u05D9\u05DE\u05EA \u05D4\u05D9\u05D0 \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4\
  \ \u05D4\u05DE\u05E9\u05DE\u05E9\u05EA \u05DC\u05E0\u05D9\u05E7\u05D5\u05D9 \u05D0\
  \u05D5 \u05E2\u05D9\u05E6\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA. \u05D1\u05D4\u05E7\u05E9\u05E8 \u05E9\u05DC\
  \ Google Apps Script, \u05E9\u05DE\u05DE\u05E9\u05E7 \u05D1\u05D0\u05D5\u05E4\u05DF\
  \ \u05E0\u05E8\u05D7\u05D1 \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 Google\
  \ \u05DB\u05DE\u05D5\u2026"
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05DE\
  \u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05D3\u05E4\u05D5\u05E1"
---

{{< edit_this_page >}}

## מה ולמה?

מחיקת תווים התואמים לתבנית מסוימת היא טכניקה המשמשת לניקוי או עיצוב מחרוזות בתכנות. בהקשר של Google Apps Script, שממשק באופן נרחב עם שירותי Google כמו Sheets וDocs, תהליך זה הופך להכרחי לאימות נתונים, הכנה והתעסקות עם נתונים, מבטיח עקביות ואמינות ברחבי מסמכים וסטים של נתונים.

## איך לעשות:

Google Apps Script מספקת שיטות רבות עוצמה לניפוי מחרוזות, ניצולת את יכולותיו הטבעיות של JavaScript. למחיקת תווים התואמים לתבנית, אנו משתמשים בregex (ביטויים רגולריים), אשר מאפשר לחפש מחרוזות עבור תבניות מסוימות ובמקרה שלנו, להסיר אותם.

הנה דוגמה מעשית:

```javascript
function removeCharacters() {
  var originalString = "123-ABC-456-DEF";
  var pattern = /[^A-Z]+/g; // Regex להתאמה לכל דבר שאינו אות רישית
  var cleanedString = originalString.replace(pattern, ""); // מסיר תווים התואמים
  
  Logger.log("מקורי: " + originalString); // מקורי: 123-ABC-456-DEF
  Logger.log("נקי: " + cleanedString); // נקי: ABCDEF
}
```

הסקריפט לעיל מגדיר תבנית להתאמה לכל תו שאינו אות רישית ומסיר אותם מהמחרוזת. זה מועיל במיוחד כאשר יש צורך לחלץ סוגים ספציפיים של נתונים (כמו אותיות בלבד) מקלט בפורמט מעורב.

## טבילה עמוקה:

השימוש בregex בניפוי מחרוזות משתרש אחורה לימי ההתפתחות הראשונים של המחשב, והתפתח ככלי עוצמתי לזיהוי תבניות בסביבות תכנות שונות, כולל Google Apps Script. למרות שregex מציע גמישות ויעילות בלתי משווה בהתאמת תבניות ומחיקת תווים, חשוב להתייחס ליישומו בזהירות. שימוש לא נכון או תבניות מורכבות ביותר עלולות להוביל לבעיות ביצועים או קוד בלתי קריא.

תוך כדי שימוש ב-Google Apps Script, היישום מנצל את שיטת `String.replace()` של JavaScript, הופכת אותו לנגישה אפילו למי שחדש ל-Apps Script אך מכיר JavaScript. עם זאת, למי שמתמודד עם סטים גדולים במיוחד של נתונים או גיליונות Google מורכבים, כדאי לשקול שיטות חלופיות או אפילו תוספים שמטפלים בעיבוד מקדים של נתונים כדי להימנע מהגבלות זמן ביצוע ולשפר יעילות סקריפט.

למרות שregex נשאר שיטה עוצמתית למחיקת תווים מבוססת תבנית, חקירת שיטות מובנות של Google Apps Script למחרוזות ומערכים למשימות פשוטות יותר או שימוש בספריות חיצוניות לתרחישים מורכבים יותר עשויים להציע פתרון מותאם יותר, מאזן בין ביצועים לתחזוקה.
