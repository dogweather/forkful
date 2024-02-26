---
date: 2024-01-27 20:36:15.727308-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D1-TypeScript \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05D0\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05E2\u05E8\u05DB\u05D9\
  \u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05D9\u05DD \u05D1\u05DC\u05EA\u05D9 \u05E6\
  \u05E4\u05D5\u05D9\u05D9\u05DD \u05D1\u05D8\u05D5\u05D5\u05D7 \u05DE\u05E1\u05D5\
  \u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E0\u05E6\u05DC\
  \u05D9\u05DD \u05D0\u05EA \u05D4\u05E1\u05E4\u05E8\u05D5\u05EA \u05D4\u05D0\u05E7\
  \u05E8\u05D0\u05D9\u05D5\u05EA \u05D4\u05DC\u05DC\u05D5 \u05DC\u05DE\u05D2\u05D5\
  \u05D5\u05DF \u05DE\u05D8\u05E8\u05D5\u05EA, \u05DB\u05D2\u05D5\u05DF \u05D9\u05E6\
  \u05D9\u05E8\u05EA\u2026"
lastmod: '2024-02-25T18:49:37.149197-07:00'
model: gpt-4-0125-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D1-TypeScript \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D0\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\
  \u05E1\u05E4\u05E8\u05D9\u05D9\u05DD \u05D1\u05DC\u05EA\u05D9 \u05E6\u05E4\u05D5\
  \u05D9\u05D9\u05DD \u05D1\u05D8\u05D5\u05D5\u05D7 \u05DE\u05E1\u05D5\u05D9\u05DD\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E0\u05E6\u05DC\u05D9\u05DD\
  \ \u05D0\u05EA \u05D4\u05E1\u05E4\u05E8\u05D5\u05EA \u05D4\u05D0\u05E7\u05E8\u05D0\
  \u05D9\u05D5\u05EA \u05D4\u05DC\u05DC\u05D5 \u05DC\u05DE\u05D2\u05D5\u05D5\u05DF\
  \ \u05DE\u05D8\u05E8\u05D5\u05EA, \u05DB\u05D2\u05D5\u05DF \u05D9\u05E6\u05D9\u05E8\
  \u05EA\u2026"
title: "\u05D2\u05D9\u05DC\u05D5\u05D9 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת מספרים אקראיים ב-TypeScript כוללת את יצירת ערכים מספריים בלתי צפויים בטווח מסוים. מתכנתים מנצלים את הספרות האקראיות הללו למגוון מטרות, כגון יצירת מזהים ייחודיים, סימולציה של נתונים לצורך בדיקות, או הוספת אלמנט של חוסר צפיות למשחקים וסימולציות.

## איך לעשות:

ב-TypeScript, ניתן ליצור מספרים אקראיים באמצעות העצם הגלובלי `Math`. להלן כמה דוגמאות מעשיות המדגימות איך ליצור מספרים אקראיים לצרכים שונים.

### יצירת מספר אקראי בסיסי

כדי ליצור מספר עשרוני אקראי בין 0 (כולל) ל-1 (לא כולל), משתמשים ב-`Math.random()`. זה לא דורש כל טיפול נוסף:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

זה עשוי להוציא ערך כמו `0.8995452185604771`.

### יצירת מספר שלם אקראי בין שני ערכים

כאשר דרוש מספר שלם בין שני ערכים מסוימים, יש לשלב את `Math.random()` וכמה פעולות חשבון:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

זה עשוי להוציא ערך שלם בין 1 ל-10, כמו `7`.

### יצירת מזהה ייחודי

ניתן לשלב מספרים אקראיים עם שיטות אחרות ליצירת מזהים ייחודיים, למשל, קטע קוד ליצירת UUID פשוט:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

זה מייצר מחרוזת הדומה ל-UUID, כמו `110e8400-e29b-41d4-a716-446655440000`.

## צלילה עמוקה

השיטה העיקרית ליצירת מספרים אקראיים ב-JavaScript ובכך גם ב-TypeScript, `Math.random()`, מבוססת על גנרטור מספרים אקראיים פסבדו (PRNG). חשוב לזכור שלמרות שהתוצאות נראות אקראיות, הן מופקות על ידי אלגוריתם דטרמיניסטי המבוסס על ערך זרע התחלתי. לכן, המספרים המופקים על ידי `Math.random()` אינם אקראיים באמת ולא צריכים להיעשות בשימוש למטרות קריפטוגרפיות.

למספרים אקראיים המאובטחים קריפטוגרפית, ממשק ה-Web Crypto API מציע את `crypto.getRandomValues()`, הגישה לה קיימת בסביבות התומכות בתקן Web Crypto, כולל דפדפנים מודרניים ו-Node.js (דרך המודול `crypto`). הנה דוגמה מהירה הממחישה את השימוש בה ב-TypeScript ליצירת מספר אקראי מאובטח בטווח:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

שיטה זו מספקת רמה חזקה יותר של אקראיות ומתאימה יותר ליישומים רגישים לאבטחה. עם זאת, היא גם דורשת יותר משאבים ועשויה לא להיות נחוצה למשימות שגרתיות יותר, כמו סימולציות פשוטות או יצירת ערכים אקראיים שאינם קריטיים.
