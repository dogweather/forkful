---
title:                "הדפסת פלט לניפוי שגיאות"
aliases:
- /he/google-apps-script/printing-debug-output/
date:                  2024-02-01T21:59:04.778597-07:00
model:                 gpt-4-0125-preview
simple_title:         "הדפסת פלט לניפוי שגיאות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/google-apps-script/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

הדפסת פלט דיבאג מערבת הנחת פקודות רישום באופן אסטרטגי בקוד שלך להצגת ערכי משתנים, זרימת ביצוע או הודעות שגיאה במהלך זמן הריצה. מתכנתים משתמשים בזה באופן נרחב לאיתור ואבחון התנהגות הסקריפטים שלהם, מבטיחים נכונות ויעילות באפליקציות של Google Apps Script.

## איך לעשות:

Google Apps Script מספקת את המחלקה `Logger` לדיבאג בסיסי, ולצרכים מתקדמים יותר, המחלקה `console` שהוצגה בזמן ריצה V8.

**שימוש ב-Logger:**

המחלקה Logger מאפשרת לך לרשום הודעות דיבאג, שתוכל לצפות בהן לאחר הביצוע בעורך ה-Apps Script תחת `View > Logs`. הנה דוגמה פשוטה:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hello, %s!", name);
}
```

לאחר הפעלת `logSample()`, תוכל לצפות בלוג עם "Hello, Wired Reader!" במציג הלוגים.

**שימוש ב-console.log עם זמן ריצה V8:**

עם זמן ריצה V8, `console.log` מספק תחביר יותר מוכר למפתחים הבאים משפות אחרות:

```javascript
function consoleSample() {
  var status = 'active';
  var count = 150;
  console.log(`Current status: ${status}, Count: ${count}`);
}
```

לאחר הביצוע, תוכל לגשת ליומני Stackdriver דרך `View > Stackdriver Logging` כדי לצפות בפלט. זה עוצמתי יותר, תומך באינטרפולציה של מחרוזות ובדיקת אובייקטים, ומשתלב עם התיעוד של Google Cloud, מציע יומנים קבועים ויכולות סינון מתקדמות.

**דוגמת פלט מ-console.log:**

```
Current status: active, Count: 150
```

## צלילה עמוקה

בהתחלה, `Logger.log` היה הכלי העיקרי לדיבאג ב-Google Apps Script, מציע דרך פשוטה וישרה להדפסת פלט לבדיקה. עם זאת, עם הופכים הסקריפטים למורכבים יותר ומשולבים עם שירותי Google Cloud Platform, הצורך בפתרון רישום יותר חזק התבהר.

נכנס זמן הריצה V8, המביא את `console.log` אל המשחק. זה לא רק מזהה את Google Apps Script עם תחביר JavaScript סטנדרטי, הופך את השפה לגישה יותר למפתחים המכירים JavaScript, אלא גם מנצל את התשתית העוצמתית של יכולות הרישום של Google Cloud. ההכנסה של `console.log` ושילובו עם Google Cloud Platform מסמנת התפתחות משמעותית ביכולות הדיבאג ב-Google Apps Script, מספקת למפתחים גישה יותר דינמית וקנה מידה גדול לפיקוח ופתרון בעיות בסקריפטים שלהם.

כאשר `Logger.log` מספיק לצרכי דיבאג בסיסיים ולפרויקטים קטנים, `console.log` עם זמן הריצה V8 מציע פתרון יותר מקיף ועתידני. זה כולל את היכולת לשמור על יומנים מעבר לסשן הביצוע, לחפש ולסנן יומנים בתוך קונסולת Google Cloud, וההתאמה הכללית עם מתודולוגיות פיתוח JavaScript מודרניות. עם זאת, מפתחים צריכים למדוד את צרכיהם כנגד המורכבות והסדר הגודל של פרויקטיהם בעת בחירה בין אפשרויות אלו.
