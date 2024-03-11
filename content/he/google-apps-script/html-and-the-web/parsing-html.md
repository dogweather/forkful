---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:07.997702-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML \u05D1- Google Apps Script \u05DB\
  \u05D5\u05DC\u05DC \u05D0\u05D9\u05EA\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05DE\u05EA\u05D5\u05DB\u05DF HTML, \u05DE\u05D4 \u05E9\u05D1\u05DE\u05D9\
  \u05D5\u05D7\u05D3 \u05E9\u05D9\u05DE\u05D5\u05E9\u05D9 \u05DB\u05D0\u05E9\u05E8\
  \ \u05DE\u05EA\u05E7\u05D9\u05D9\u05DE\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\
  \u05E7\u05E6\u05D9\u05D4 \u05E2\u05DD \u05D3\u05E4\u05D9 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D0\u05D5 \u05DE\u05E7\u05D5\u05E8\u05D5\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DE\u05D1\u05D5\u05E1\u05E1\u05D9 \u05E8\u05E9\u05EA\
  .\u2026"
lastmod: '2024-03-11T00:14:11.972646-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML \u05D1- Google Apps Script \u05DB\u05D5\
  \u05DC\u05DC \u05D0\u05D9\u05EA\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05DE\u05EA\u05D5\u05DB\u05DF HTML, \u05DE\u05D4 \u05E9\u05D1\u05DE\u05D9\u05D5\
  \u05D7\u05D3 \u05E9\u05D9\u05DE\u05D5\u05E9\u05D9 \u05DB\u05D0\u05E9\u05E8 \u05DE\
  \u05EA\u05E7\u05D9\u05D9\u05DE\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05E6\
  \u05D9\u05D4 \u05E2\u05DD \u05D3\u05E4\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\
  \u05D8 \u05D0\u05D5 \u05DE\u05E7\u05D5\u05E8\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05DE\u05D1\u05D5\u05E1\u05E1\u05D9 \u05E8\u05E9\u05EA.\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?
ניתוח HTML ב- Google Apps Script כולל איתור נתונים מתוכן HTML, מה שבמיוחד שימושי כאשר מתקיימת אינטראקציה עם דפי אינטרנט או מקורות נתונים מבוססי רשת. מתכנתים עושים זאת כדי לאוטמט את איסוף הנתונים, לתמר תוכן אינטרנטי, או לשלב פונקציונליות מהרשת עם אפליקציות גוגל כמו Sheets ו- Docs.

## איך לעשות:
ל- Google Apps Script אין שיטה מובנית לניתוח HTML. עם זאת, אפשר לנצל את השירות `UrlFetchApp` לאחזור תוכן HTML ולאחר מכן להשתמש בשיטות JavaScript או regex (ביטויים רגולריים) לניתוח. למטה דוגמה בסיסית לאיך לאחזר ולנתח את תג הכותרת מדף אינטרנט.

```javascript
function parseHTMLTitle(url) {
  // אחזר את התוכן HTML של האתר
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // השתמש בביטוי רגולרי פשוט למציאת התוכן של תג <title>
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // בדוק אם נמצאה כותרת והחזר אותה
  if (match && match.length > 1) {
    return match[1];
  }

  return 'לא נמצאה כותרת';
}

// שימוש בדוגמה
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // פלט את כותרת הדף
```

לניתוח HTML מתוחכם יותר, אפשר להשתמש ב-`XmlService` לניתוח ה-HTML כ-XM‪L. יחד עם זאת, זו דורשת שה-HTML יהיה מסודר כ-XM‪L, שלא תמיד קורה:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // מכאן, לנווט בעץ ה-XML עם שיטות XmlService
    // לדוגמה, למצוא אלמנט או תכונה מסוימים
  } catch(e) {
    Logger.log('שגיאה בניתוח HTML: ' + e.toString());
  }
}
```

## צלילה עמוקה:
באופן היסטורי, ניתוח HTML בסביבות כמו Google Apps Script היה מאתגר בשל החסרון של מודל אובייקטים של המסמך (DOM) או ספריות ניתוח מיוחדות שהן נפוצות בהקשרים תכנותיים אחרים. JavaScript בדפדפן, לדוגמה, זמין עם ה-DOM באופן ישיר, וסביבות Node.js יש גישה למגוון חבילות NPM כמו `cheerio` או `jsdom` לניתוח HTML.

הגישה של Google Apps Script מתמקדת בעיקר בשימוש ב-`UrlFetchApp` לבקשות רשת ולאחר מכן בעיבוד הנתונים מהתגובה באמצעות ביטויים רגולריים או שיטות ניתוח XML. אף על פי שביטויים רגולריים יכולים להיות שימושיים למשימות ניתוח פשוטות, בדרך כלל לא מומלץ להשתמש בהם לניתוח HTML מורכב בשל סיכון לטעויות ולביטחון נמוך של הקוד. ניתוח XML עם `XmlService` מציע גישה מובנית יותר אבל דורש HTML/XML מסודר טוב, שיכולה להיות מגבלה כאשר מתמודדים עם דפי אינטרנט שרירותיים.

לצרכי ניתוח מורכבים או כאשר מתמודדים עם HTML לא מסודר היטב, אסטרטגיה חלופית יכולה לכלול שימוש בשירות רשת חיצוני ל- Google Apps Script. שירות זה יכול לעבד תוכן HTML, אולי באמצעות שיטת ניתוח חזקה יותר או ספרייה, ולאחר מכן להחזיר את הנתונים המעובדים בצורה שקל להתמקד בה על ידי Google Apps Script. גישה זו, עם זאת, מביאה עמה איטיות רשת ומורכבות של ניהול שירות רשת נוסף.

למרות אתגרים אלו, ניתוח HTML בתוך Google Apps Script נשאר כלי חזק, במיוחד כאשר משלבים עם שירותי גוגל ו-APIs אחרים, מספק מגוון אפשרויות אוטומציה שיכולות לשפר משמעותית את הפרודוקטיביות ואת יכולות עיבוד הנתונים.
