---
title:                "חיפוש והחלפה של טקסט"
aliases:
- he/google-apps-script/searching-and-replacing-text.md
date:                  2024-02-01T22:02:21.299009-07:00
model:                 gpt-4-0125-preview
simple_title:         "חיפוש והחלפה של טקסט"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפת טקסט ב-Google Apps Script כולל איתור תכני טקסט מסוימים באופן תכנותי במסמך, גיליון אלקטרוני, או כל סוג אחר של תוכן ב-Google Apps, והחלפתם בערכי טקסט אחרים. מתכנתים מנצלים את הפונקציונליות הזו כדי לאטום את עריכת כמויות גדולות של תוכן, לתקן שגיאות נפוצות, לתקנן מונחים לפני כל המסמכים, או להכניס נתונים דינמיים לתבניות.

## איך לעשות:

Google Apps Script מציע דרך ישירה לחיפוש והחלפה טקסט, במיוחד בתוך Google Docs ו-Sheets. להלן דוגמאות לשניהם.

### Google Docs:

לחיפוש והחלפת טקסט במסמך של Google, תתקשר בעיקר עם המחלקה `DocumentApp`.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // לחיפוש והחלפת משפט מסוים
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// שימוש
searchReplaceInDoc();
```

חתיכת קוד זו מחפשת את כל המופעים של `'searchText'` במסמך הפעיל של Google ומחליפה אותם ב-`'replacementText'`.

### Google Sheets:

באופן דומה, ב-Google Sheets, אתה יכול להשתמש ב-`SpreadsheetApp` כדי לבצע פעולות חיפוש והחלפה:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // חיפוש והחלפה בגיליון הפעיל כעת
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// שימוש
searchReplaceInSheet();
```

בדוגמה זו, `createTextFinder('searchText')` מחפשת את 'searchText' בגיליון הפעיל, ו-`replaceAllWith('replacementText')` מחליפה את כל ההופעות ב-'replacementText'.

## עיון עמוק

היכולת לחפש ולהחליף ב-Google Apps Script מושפעת במידה רבה מטבעה המבוסס רשת שלה, מה שמאפשר לסקריפטים לשנות טקסט בצורה חלקה ברחבי מגוון יישומי Google. מבחינה היסטורית, יכולת זו נובעת מההקשר הרחב יותר של עיבוד ושינוי טקסט בתכנות, שבו ביטויים רגולריים ופונקציות מחרוזת בשפות כמו Perl ו-Python הציבו סטנדרט גבוה לגמישות ועוצמה.

למרות שפונקציונליות החיפוש וההחלפה של Google Apps Script עוצמתית עבור החלפות ישירות, היא חסרה את היכולת המלאה של ביטויים רגולריים שנמצאת בחלק מהשפות האחרות. לדוגמה, בעוד שניתן להשתמש בביטויים רגולריים בסיסיים ב-`createTextFinder` ב-Google Sheets, האפשרויות לתיאום ולשינוי דפוסים מורכבים מוגבלות בהשוואה ל-Perl או Python.

לצרכי עיבוד טקסט מתקדמים יותר, תכנתים עשויים להשתמש בייצוא של תוכן מ-Google Docs או Sheets לפורמט שניתן לעבד מחוץ לכך בשפות חזקות יותר, או להשתמש ב-Google Apps Script כדי לקרוא ל-APIs או שירותים חיצוניים המציעים יכולות מתקדמות יותר של שינוי טקסט.

למרות המגבלות הללו, עבור רוב המשימות האופייניות של חיפוש והחלפה בתוך אקוסיסטם של Google Apps, Google Apps Script מציע פתרון פשוט, יעיל ומאוד משולב, המותאם לצרכים של אוטומציה ותסריטים בתוך סוויטת כלי הפרודוקטיביות של Google.
