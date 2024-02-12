---
title:                "קבלת התאריך הנוכחי"
aliases: - /he/google-apps-script/getting-the-current-date.md
date:                  2024-02-01T21:55:41.287690-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/getting-the-current-date.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

קבלת התאריך הנוכחי ב-Google Apps Script היא אודות לקיחת התאריך והשעה החיים, משימה נפוצה לאוטומציה של משימות, רישום, והוספת חותם זמן באפליקציות הקשורות לאקוסיסטם של גוגל. מתכנתים משתמשים בזה ליצירת תוכן דינמי, מעקב אחר מועדי סיום, ותיזמון בתוך Google Docs, Sheets, ושירותי גוגל אחרים.

## איך ל:

Google Apps Script, המבוסס על JavaScript, מציע שיטות פשוטות לקבלת התאריך הנוכחי. ניתן להשתמש בבונה `new Date()` כדי ליצור אובייקט תאריך חדש המייצג את התאריך והשעה הנוכחיים. כך תוכלו לעבד ולהציג את זה בפורמטים שונים.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // רושם את התאריך והשעה הנוכחיים באזור הזמן של הסקריפט
  
  // להצגת התאריך בלבד בפורמט YYYY-MM-DD
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // דוגמא לפלט: "2023-04-01"
  
  // הצגת זה בפורמט קריא יותר
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // דוגמא לפלט: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

קטעי הקוד האלה מדגימים איך לתפוס ולעצב את התאריך והשעה הנוכחיים, מציגים גמישות לצרכים תכנותיים שונים בתוך Google Apps Script.

## צלילה עמוקה

לפני ש-JavaScript הסתדר על אובייקט ה`Date`, מתכנתים היו צריכים לנהל ידנית זמן ותאריך דרך אמצעים פחות סטנדרטיים וכבדים יותר. זה כלל השימוש במספרים שמייצגים תגים זמניים ופונקציות תאריך בנויות בבית, אשר נבדלו מסביבה תכנותית לשנייה, מה שהוביל לאי-עקביות ובעיות תאימות.

הצגת אובייקט ה`new Date()` ב-JavaScript, ובהרחבה ב-Google Apps Script, סטנדרטיזציה של פעולות תאריך וזמן, הפכה אותם ליותר אינטואיטיביים והקטינה את הכמות של קוד הנדרשת לפעולות הקשורות לתאריך. כדאי לציין שבעוד שהמימוש של Google Apps Script נוח ומספיק לעתים רבות ליישומים בתוך מוצרי גוגל, הוא עשוי שלא להתאים לכל התסריטים, במיוחד אלו הדורשים טיפול מורכב באזורי זמן או רישום זמן מדויק בסביבות עמוסות.

למקרים מתקדמים כאלו, מתכנתים לעיתים קרובות פונים לספריות כמו Moment.js או date-fns ב-JavaScript. למרות ש-Google Apps Script לא תומך באופן טבעי בספריות אלו, מפתחים יכולים לחקות חלק מהפונקציונליות שלהם באמצעות שימוש בשיטות התאריך של JavaScript הזמינות או על ידי גישה לספריות חיצוניות דרך שירותי HTML Service או URL Fetch של Apps Script. למרות אלטרנטיבות אלו, פשטות השימוש והאינטגרציה של פונקציות התאריך והזמן הטבעיות של Google Apps Script נשארות הבחירה הראשונית לרוב משימות תוך אקוסיסטם גוגל.
