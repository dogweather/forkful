---
title:                "טיפול בשגיאות"
date:                  2024-02-01T21:56:04.730578-07:00
model:                 gpt-4-0125-preview
simple_title:         "טיפול בשגיאות"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/handling-errors.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

טיפול בשגיאות ב-Google Apps Script עוסק בחיזוי, תפיסה, והגיבה לחריגים או שגיאות שמתרחשות במהלך ביצוע הסקריפט. מתכנתים מיישמים זאת כדי להגן על סקריפטים מפני כשלונות לא צפויים, ולהבטיח אפליקציות חלקות וידידותיות למשתמש שיכולות לנהל או לרשום שגיאות בחן מבלי לקרוס בפתאומיות.

## איך לעשות:

Google Apps Script, המבוסס על ג'אווהסקריפט, מאפשר לנו להשתמש בביטוי `try-catch` המסורתי לטיפול בשגיאות, יחד עם `finally` אם נדרש ניקוי גם כן, בלי תלות אם הפעולה הסתיימה בהצלחה או בשגיאה.

```javascript
function myFunction() {
  try {
    // קוד שעלול לגרום לשגיאה
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("תא A1 ריק.");
    }
    Logger.log(data);
  } catch (e) {
    // קוד לטיפול בשגיאה
    Logger.log("שגיאה: " + e.message);
  } finally {
    // קוד לניקוי, מתבצע בין אם קרתה שגיאה ובין אם לא
    Logger.log("הפונקציה הושלמה.");
  }
}
```

דוגמת פלט ללא שגיאה:
```
[ערך התא]
הפונקציה הושלמה.
```

דוגמת פלט עם שגיאה (בהנחה ש-A1 ריק):
```
שגיאה: תא A1 ריק.
הפונקציה הושלמה.
```

Google Apps Script גם תומך בזריקת שגיאות מותאמות אישית באמצעות האובייקט `Error` ובתפיסת סוגים ספציפיים של שגיאות אם נדרש. עם זאת, חוסר בקטגוריזציה מתקדמת של שגיאות מחייב תלות בהודעות השגיאה למטרת הפרטיות.

## צלילה עמוקה

היסטורית, טיפול בשגיאות בשפות סקריפט כמו ג'אווהסקריפט (ובהרחבה, Google Apps Script) היה פחות מתוחכם ביחס לחלק מהשפות המהודרות, שמציעות תכונות כמו היררכיות חריגים מפורטות וכלים מקיפים לניפוי באגים. מודל Google Apps Script יחסית פשוט, מנצל את פרדיגמת `try-catch-finally` של ג'אווהסקריפט. הפשטות הזו מתיישרת עם מטרת השפה לפתח ולהפיץ במהירות אפליקציות בקנה מידה קטן עד בינוני בתוך אקוסיסטם של גוגל, אבל לעיתים עלולה להגביל מפתחים המתמודדים עם תרחישי שגיאות מורכבים.

באפליקציות יותר מורכבות, מתכנתים לעיתים קרובות משלימים את טיפול השגיאות הטבעי של Google Apps Script עם מנגנוני רישום ודיווח שגיאות מותאמים אישית. זה יכול לכלול כתיבת שגיאות לגליון Google למטרות ביקורת או שימוש בשירותי רישום צד שלישי דרך שירותי Fetch URL של Google Apps Script לשליחת פרטי השגיאה מחוץ לסביבת הסקריפט.

אף על פי ש-Google Apps Script עשוי להישאר מאחורי שפות כמו ג'אווה או C# בנוגע למורכבות וליכולות של טיפול בשגיאות מובנות, השילוב שלו עם שירותי גוגל והפשטות של גישת `try-catch-finally` מהווים כלי עוצמתי למפתחים לאוטומציה מהירה של משימות ויצירת אינטגרציות בתוך אקוסיסטם של גוגל. מפתחים מרקעים אחרים עשויים למצוא את האתגר לא בשליטה בדפוסי טיפול בשגיאות מורכבים, אלא בניצול יצירתי של מה שזמין כדי להבטיח שהסקריפטים שלהם חסונים ונוחים למשתמש.