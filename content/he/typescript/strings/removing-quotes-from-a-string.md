---
title:                "הסרת מרכאות ממחרוזת"
aliases:
- /he/typescript/removing-quotes-from-a-string.md
date:                  2024-01-26T03:43:38.164729-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/removing-quotes-from-a-string.md"
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
