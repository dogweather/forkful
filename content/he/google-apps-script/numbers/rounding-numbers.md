---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:03.286055-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  , \u05DE\u05D5\u05E9\u05D2 \u05D9\u05E1\u05D5\u05D3\u05D9 \u05D1\u05EA\u05DB\u05E0\
  \u05D5\u05EA \u05DE\u05D7\u05E9\u05D1\u05D9\u05DD, \u05DB\u05D5\u05DC\u05DC \u05D4\
  \u05EA\u05D0\u05DE\u05D4 \u05E9\u05DC \u05DE\u05E1\u05E4\u05E8 \u05DC\u05E9\u05DC\
  \u05DD \u05D4\u05E7\u05E8\u05D5\u05D1 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D0\u05DC\
  \u05D9\u05D5 \u05D0\u05D5 \u05DC\u05DE\u05E1\u05E4\u05E8 \u05DE\u05D5\u05D2\u05D3\
  \u05E8 \u05E9\u05DC \u05DE\u05E7\u05D5\u05DE\u05D5\u05EA \u05E2\u05E9\u05E8\u05D5\
  \u05E0\u05D9\u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E0\u05D5\u05D4\
  \u05D2\u05D9\u05DD \u05DC\u05D1\u05E6\u05E2 \u05E2\u05D9\u05D2\u05D5\u05DC \u05DB\
  \u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8\u2026"
lastmod: '2024-02-25T18:49:36.868032-07:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD, \u05DE\
  \u05D5\u05E9\u05D2 \u05D9\u05E1\u05D5\u05D3\u05D9 \u05D1\u05EA\u05DB\u05E0\u05D5\
  \u05EA \u05DE\u05D7\u05E9\u05D1\u05D9\u05DD, \u05DB\u05D5\u05DC\u05DC \u05D4\u05EA\
  \u05D0\u05DE\u05D4 \u05E9\u05DC \u05DE\u05E1\u05E4\u05E8 \u05DC\u05E9\u05DC\u05DD\
  \ \u05D4\u05E7\u05E8\u05D5\u05D1 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D0\u05DC\u05D9\
  \u05D5 \u05D0\u05D5 \u05DC\u05DE\u05E1\u05E4\u05E8 \u05DE\u05D5\u05D2\u05D3\u05E8\
  \ \u05E9\u05DC \u05DE\u05E7\u05D5\u05DE\u05D5\u05EA \u05E2\u05E9\u05E8\u05D5\u05E0\
  \u05D9\u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E0\u05D5\u05D4\u05D2\
  \u05D9\u05DD \u05DC\u05D1\u05E6\u05E2 \u05E2\u05D9\u05D2\u05D5\u05DC \u05DB\u05D3\
  \u05D9 \u05DC\u05E4\u05E9\u05D8\u2026"
title: "\u05E1\u05D9\u05D1\u05D5\u05D1 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

עיגול מספרים, מושג יסודי בתכנות מחשבים, כולל התאמה של מספר לשלם הקרוב ביותר אליו או למספר מוגדר של מקומות עשרוניים. תכנתים נוהגים לבצע עיגול כדי לפשט מספרים לקריאות אנושית או לעמוד בצרכים מסוימים של חישובים, מה שמבטיח דיוק ומפחית את נטל החישוב.

## איך:

Google Apps Script, המבוסס על JavaScript, מציע שיטות סטנדרטיות לעיגול מספרים. הנה ניתוח של שלוש טכניקות שנפוצות בשימוש:

### Math.round()
פונקציה זו מעגלת מספר לשלם הקרוב ביותר.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // מפרסם: 3
```

### Math.ceil()
מעגלת מספר לשלם הגבוה ביותר הקרוב.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // מפרסם: 3
```

### Math.floor()
בניגוד, מעגלת מספר לשלם הנמוך ביותר הקרוב.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // מפרסם: 2
```

למקומות עשרוניים ספציפיים, ניתן להשתמש ב-`.toFixed()`, שלמעשה מחזיר מחרוזת, או בגישה יותר מורכבת לעיגול מתמטי:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // מפרסם: "2.57" (כמחרוזת)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // מפרסם: 2.57
```

## צלילה לעומק

עיגול מספרים ב-Google Apps Script אינו שונה כל כך מהאופן שבו זה נעשה בסביבות JavaScript אחרות. עם זאת, חשוב להבין את ההבדלים בשיטות העיגול ואת הפוטנציאל לבעיות בחישובי אריתמטיקה נקודה צפה. לדוגמה, בשל הדרך שבה מחשבים מייצגים מספרים צפים, לא כל השברים העשרוניים יכולים להיות מיוצגים בדיוק מושלם, מה שיכול להוביל לתוצאות עיגול לעיתים לא צפויות.

באופן היסטורי, JavaScript (וברחבה, Google Apps Script) מתמודד עם זה על ידי התאמה לתקן IEEE 754, שבו משתמשות שפות תכנות רבות אחרות לחישובי אריתמטיקת נקודה צפה. תקן זה קובע איך מספרים מתעגלים, מה שמבטיח עקביות ברחבי פלטפורמות ושפות שונות.

למרות ששיטות העיגול הישירות ב-Google Apps Script הן פשוטות ולעיתים קרובות מספיקות, יישומים מורכבים או כאלה הדורשים דיוק גבוה עשויים להפיק תועלת מספריות כמו decimal.js או big.js, שתוכננו להתמודד עם אריתמטיקה בדיוק שרירותי. אלו יכולות להיות שימושיות במיוחד כאשר עובדים עם חישובים פיננסים או מדעיים, שבהם דיוק המספרים המעוגלים הוא בעל חשיבות רבה.

זכרו, למרות זאת, ששימוש בספריות חיצוניות ב-Google Apps Script דורש טעינתן דרך עורך הסקריפטים, דבר שעשוי להכניס תלותיות או להשפיע על ביצועי הסקריפט שלכם תלוי באופן השימוש. במקרים רבים, שיטות ה-Math הפנימיות הן לחלוטין מספיקות, אבל עבור תרחישים יוצאי דופן הדורשים דיוק עד למעלה ה-n, הבטחה מעבר לספריה הסטנדרטית יכולה להיות נחוצה.
