---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:05.796242-07:00
description: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1-Google Apps Script \u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05D4\u05D8\u05DE\u05E2\u05D4 \u05D3\u05D9\u05E0\
  \u05DE\u05D9\u05EA \u05E9\u05DC \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05D5\u05D1\u05DB\
  \u05DA \u05DE\u05E7\u05DC\u05D4 \u05E2\u05DC \u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\
  \u05D5\u05D3 \u05E7\u05E8\u05D9\u05D0 \u05D5\u05E0\u05D9\u05EA\u05DF \u05DC\u05EA\
  \u05D7\u05D6\u05D5\u05E7\u05D4 \u05D9\u05D5\u05EA\u05E8. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.540757-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1-Google Apps Script \u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05D4\u05D8\u05DE\u05E2\u05D4 \u05D3\u05D9\u05E0\
  \u05DE\u05D9\u05EA \u05E9\u05DC \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05D5\u05D1\u05DB\
  \u05DA \u05DE\u05E7\u05DC\u05D4 \u05E2\u05DC \u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\
  \u05D5\u05D3 \u05E7\u05E8\u05D9\u05D0 \u05D5\u05E0\u05D9\u05EA\u05DF \u05DC\u05EA\
  \u05D7\u05D6\u05D5\u05E7\u05D4 \u05D9\u05D5\u05EA\u05E8. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\u2026"
title: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

אינטרפולציה של מחרוזות ב-Google Apps Script מאפשרת הטמעה דינמית של ביטויים בתוך מחרוזות, ובכך מקלה על יצירת קוד קריא וניתן לתחזוקה יותר. מתכנתים משתמשים בטכניקה זו כדי לשלב באופן חלק משתנים וביטויים בתוך מחרוזות ללא הצורך בשימוש בתחביר התמזגות המסורבל.

## איך לעשות:

ב-Google Apps Script, אינטרפולציה של מחרוזות מתבצעת דרך ליטרלי תבניות. אלה הם ליטרלי מחרוזות המאפשרים הטמעת ביטויים, המסומנים באמצעות גרשיים חוזרים (`) במקום באמצעות הציטוטים הרגילים. כך תוכלו להשתמש בהם:

```javascript
// דוגמה בסיסית
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`שלום, ${user}!`); // פלט: שלום, Alice!
}

// השימוש בביטויים
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`חמישה ועשר הם ${a + b}.`); // פלט: חמישה ועשר הם 15.
}

// מחרוזות רב-שורה
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`זו מחרוזת רב-שורה:
שלום לכולם,
נושא הדיון שלנו היום הוא ${item}.`);
  // פלט:
  // זו מחרוזת רב-שורה:
  // שלום לכולם,
  // נושא הדיון שלנו היום הוא Google Apps Script.
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

הדוגמאות האלה מדגימות שימוש בסיסי, הטמעת ביטויים, ויצירת מחרוזות רב-שורה עם ערכים מוטמעים.

## חקירה עמוקה יותר

ליטרלי תבניות, כולל אינטרפולציה של מחרוזות, הוצגו ב-ECMAScript 2015 (ES6) ולאחר מכן הוטמעו ב-Google Apps Script. לפני כן, מתכנתים נאלצו להסתמך כולם על הדבקת מחרוזות, שיכולה להיות מסורבלת עבור מחרוזות מורכבות או כאשר משלבים ערכים של מספר משתנים.

```javascript
// הדרך הישנה (לפני ES6)
var user = 'Bob';
console.log('שלום, ' + user + '!');
```

למרות שאינטרפולציה של מחרוזות היא תכונה חזקה, חשוב להיות מודעים להקשרים בהם היא נמצאת בשימוש. למשל, הטמעה ישירה של קלט משתמש ללא טיהור ראוי עלולה להוביל לבעיות אבטחת מידע, כמו התקפות הזרקה. מתכנתי Google Apps Script צריכים לוודא שכל תוכן דינמי המוטמע במחרוזות עובר בדיקה או טיהור הולם.

בהשוואה לשפות תכנות אחרות, המושג של אינטרפולציה של מחרוזות קיים ברחבי, עם תחביר משתנה. פייתון משתמשת ב-f-strings או בשיטת ה-`format`, רובי משתמשת ב-`#{}` בתוך מחרוזות עם ציטוטיים כפולים, ושפות רבות מודרניות אימצו תכונות דומות בשל הקריאות והנוחות שהן מציעות.

למרות ש-Google Apps Script לא מציעה תכונות אינטרפולציה נוספות מעבר לאלה המסופקות על ידי התקני ECMAScript, הפונקציונליות הנוכחית חזקה ומספיקה לרוב המקרים. מפתחים הבאים משפות עם מנגנוני אינטרפולציה יותר מורכבים עשויים לצפות להתאמה, אך ככל הנראה יעריכו את הפשטות והיעילות של ליטרלי תבניות ב-Google Apps Script.
