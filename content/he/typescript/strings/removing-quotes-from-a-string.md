---
aliases:
- /he/typescript/removing-quotes-from-a-string/
date: 2024-01-26 03:43:38.164729-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD\
  \ \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4\
  \ \u05DC\u05D4\u05E1\u05D9\u05E8 \u05D0\u05EA \u05EA\u05D5\u05D5\u05D9 \u05D4\u05E6\
  \u05D9\u05D8\u05D5\u05D8 \u05D4\u05D9\u05D7\u05D9\u05D3 (`'`) \u05D0\u05D5 \u05D4\
  \u05DB\u05E4\u05D5\u05DC (`\"`) \u05E9\u05DE\u05D2\u05D3\u05D9\u05E8\u05D9\u05DD\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D5\u05D3 \u05D1\u05EA\u05D5\u05DA\
  \ \u05D4\u05E7\u05D5\u05D3. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DE\u05E1\u05D9\u05D1\u05D5\u05EA \u05E9\u05D5\
  \u05E0\u05D5\u05EA, \u05DB\u05D2\u05D5\u05DF \u05E2\u05D9\u05E6\u05D5\u05D1 \u05E4\
  \u05DC\u05D8,\u2026"
lastmod: 2024-02-18 23:08:52.557686
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4 \u05DC\
  \u05D4\u05E1\u05D9\u05E8 \u05D0\u05EA \u05EA\u05D5\u05D5\u05D9 \u05D4\u05E6\u05D9\
  \u05D8\u05D5\u05D8 \u05D4\u05D9\u05D7\u05D9\u05D3 (`'`) \u05D0\u05D5 \u05D4\u05DB\
  \u05E4\u05D5\u05DC (`\"`) \u05E9\u05DE\u05D2\u05D3\u05D9\u05E8\u05D9\u05DD \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D5\u05D3 \u05D1\u05EA\u05D5\u05DA \u05D4\
  \u05E7\u05D5\u05D3. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DE\u05E1\u05D9\u05D1\u05D5\u05EA \u05E9\u05D5\u05E0\
  \u05D5\u05EA, \u05DB\u05D2\u05D5\u05DF \u05E2\u05D9\u05E6\u05D5\u05D1 \u05E4\u05DC\
  \u05D8,\u2026"
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת ציטוטים ממחרוזת פירושה להסיר את תווי הציטוט היחיד (`'`) או הכפול (`"`) שמגדירים מחרוזת קוד בתוך הקוד. תכנתים עושים זאת מסיבות שונות, כגון עיצוב פלט, סניטציה של קלט משתמש, או הכנת מחרוזות לפיענוח או אחסון שבהם הציטוטים אינם נחוצים או עשויים לגרום לשגיאות.

## איך לעשות זאת:
הנה המדריך הפשוט שלך להסרת סימני הציטוט המעצבנים האלו מהמחרוזות שלך ב-TypeScript.

```typescript
// אפשרות א': החלפת ציטוטים יחידים או כפולים באמצעות regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"מחרוזת בציטוט"`)); // מחרוזת בציטוט
console.log(removeQuotes(`'עוד אחת'`)); // עוד אחת

// אפשרות ב': טיפול במחרוזות שמתחילות ונגמרות עם ציטוטים שונים
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"לא מתאימים'`)); // "לא מתאימים'

// אפשרות ג': חיתוך סוגים שונים של ציטוטים
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'מיקס ומאץ'"`)); // מיקס ומאץ
```

## צלילה עמוקה
עוד לפני ש-TypeScript היה בכלל דבר, מתכנתי JavaScript כבר התמודדו עם טריקים של ציטוטים, והסיפור זהה למדי גם עבור TypeScript. ככל שהזמן מתקדם, כך גם הדרך בה אנו חותכים מחרוזות. כיום, עם העוצמה של regex, אנו דוחקים בצד את החיתוך המגושם של מחרוזות או שיטות מייגעות אחרות.

למרות שהדוגמאות שלעיל אמורות לכסות את רוב הצורכים שלך, זכור, ציטוטים יכולים להיות מורכבים. ציטוטים מקוננים, לא מתאימים ומומלטים הם התחבולנים הממתינים להפיל אותך. עבור אלו, ייתכן שתצטרך תבניות מתוחכמות יותר או אפילו מפענחים לטיפול בכל מקרה מסובך.

אלטרנטיבות? ישנם אנשים שאוהבים להשתמש בספריות כמו lodash, עם מתודות כמו `trim` ו-`trimStart` / `trimEnd`, שניתן להתאים אותן לחיתוך ציטוטים אם תגדיר את התווים שאתה רוצה לקצץ.

ולאוהבי TypeScript, בואו לא נשכח את הטיפוסים. כאן אנו מתמקדים בעיקר במחרוזות, אבל כאשר אתה עובד עם קלט משתמש או פיענוח, הכנסת בדיקות טיפוס או אפילו ג'נריקים יכולה לעזור להבטיח שהקוד שלך יישאר בטוח כמו שהציטוטים שלך חתוכים.

## ראה גם
בדוק את האתרים הווירטואליים הבאים למידע נוסף:

- MDN Web Docs על regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- התיעוד הרשמי של TypeScript (https://www.typescriptlang.org/docs/)
- אינך זקוק ל-Lodash/Underscore – עזרים למחרוזות (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: חצה את התעלות שבהן מתכנתים רבים נאבקו באסונות של ציטוטים (https://stackoverflow.com/)
