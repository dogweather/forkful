---
date: 2024-01-26 03:41:20.172806-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D5\u05DE\u05E8\u05EA \u05DC\u05D4\
  \u05D9\u05E4\u05D8\u05E8 \u05DE\u05E1\u05D9\u05DE\u05E0\u05D9 \u05D4\u05E6\u05D9\
  \u05D8\u05D5\u05D8 \u05D4\u05DE\u05E6\u05D9\u05E7\u05D9\u05DD \u05D0\u05E9\u05E8\
  \ \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\u05D1\u05DC\u05D1\u05DC \u05D0\u05EA\
  \ \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA, \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3\
  \ \u05DB\u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05DE\u05E0\u05EA\u05D7 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DE\u05D1\u05E0\u05D4 \u05D0\u05D5\u05D1\
  \u05D9\u05D9\u05E7\u05D8\u05D9\u05DD \u05E9\u05DC JSON. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-11T00:14:13.450743-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D5\u05DE\u05E8\u05EA \u05DC\u05D4\u05D9\
  \u05E4\u05D8\u05E8 \u05DE\u05E1\u05D9\u05DE\u05E0\u05D9 \u05D4\u05E6\u05D9\u05D8\
  \u05D5\u05D8 \u05D4\u05DE\u05E6\u05D9\u05E7\u05D9\u05DD \u05D0\u05E9\u05E8 \u05D9\
  \u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\u05D1\u05DC\u05D1\u05DC \u05D0\u05EA \u05D4\
  \u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA, \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3 \u05DB\
  \u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05DE\u05E0\u05EA\u05D7 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D0\u05D5 \u05DE\u05D1\u05E0\u05D4 \u05D0\u05D5\u05D1\u05D9\
  \u05D9\u05E7\u05D8\u05D9\u05DD \u05E9\u05DC JSON. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\u2026"
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת מרכאות ממחרוזת אומרת להיפטר מסימני הציטוט המציקים אשר יכולים לבלבל את הקוד שלך, במיוחד כאשר אתה מנתח נתונים או מבנה אובייקטים של JSON. מתכנתים עושים זאת כדי לנקות קלטים, למנוע שגיאות תחביר ולהפוך מחרוזות למתאימות יותר לחלקים אחרים של הקוד שלהם.

## איך לעשות:
דמיין שיש לך מחרוזת שכלואה במרכאות כפולות, כמו `"\"שלום, עולם!\""` ואתה רוצה את הטקסט הטהור, ללא מרכאות. הנה קטע קוד קצר בJavaScript שיחרר את מחרוזתך מאזיקי המרכאות הללו:

```javascript
let quotedString = "\"שלום, עולם!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // פלט: שלום, עולם!
```

ואם אתה מתמודד עם מרכאות יחידות? רק תשתף את הביטוי הרגולרי קצת:

```javascript
let singleQuotedString = "'שלום, עולם!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // פלט: שלום, עולם!
```

או מה אם המחרוזת שלך היא מיקס של שניהם? אין בעיה:

```javascript
let mixedQuotedString = "\"'שלום, עולם!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // פלט: 'שלום, עולם!'
```

## עיון מעמיק
לפני שJSON תפס כל כך הרבה מקום, בריחת מרכאות היתה מערב הפרוע של קווי נטייה וטריקים. השפות התכנותיות הראשונות לא תמיד הסתדרו טוב עם מרכאות, שהביא להרבה עיבוד ידני של מחרוזות. כעת, עם פורמטים מתוקנים של נתונים, הסרת מרכאות לעיתים קרובות נוגעת לניקוי קלטים לפני שהם מעובדים כJSON או שמירת טקסט ללא קונפליקטים של עיצוב.

חלופות ל`.replace()`? בטח! אתה יכול לפצל ולחבר מחדש מחרוזת על בסיס מרכאות, להשתמש בslice אם אתה בטוח במיקומי המרכאות שלך, או אפילו בצירוף regex להוציא את הטקסט הדרוש. הכול תלוי בהקשר.

אבל אל תשכח ממקרי קצה: מרכאות בתוך מרכאות, מרכאות המוברחות ותווים בינלאומיים. חשוב על המחרוזת שלך כעל שדה מוקשים פוטנציאלי של יוצאי דופן, וצעד בזהירות. מנועי JavaScript מודרניים מאופטמים לעבוד ביעילות עם פעולות regex, כך שבדרך כלל הם הבחירה הנפוצה, אבל תמיד שווה לבדוק ביצועים עבור משימות עיבוד נתונים כבדות.

## ראה גם
תחקור עומק יותר את עיבוד מחרוזות וביטויים רגולריים:

- רשת המפתחים של מוזילה על String.replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 לבדוק את תבניות הregex שלך: https://regex101.com/
- JSON.org כדי להבין למה אנו מתמודדים עם כל כך הרבה מרכאות בפיתוח האינטרנט המודרני: http://json.org/
