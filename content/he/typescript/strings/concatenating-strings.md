---
title:                "שרשור מחרוזות"
aliases: - /he/typescript/concatenating-strings.md
date:                  2024-01-20T17:36:24.794599-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
צירופי מחרוזות הוא פשוט לקחת שתי מחרוזות (או יותר) ולהדביק אותן יחד לאחת. תכנתים שעושים זאת כדי ליצור טקסט משולב, לבנות משפטים דינמיים, או להוסיף מידע לפלט.

## איך לעשות:
```typescript
let greeting = 'שלום';
let target = 'עולם';
let message = greeting + ', ' + target + '!'; // צירוף עם אופרטור +
console.log(message); // "שלום, עולם!"

// עם תבנית מיתר (Template Literals)
message = `${greeting}, ${target}!`;
console.log(message); // "שלום, עולם!"

// צירוף מערך של מחרוזות עם מפריד
let colors = ["אדום", "ירוק", "כחול"];
let listOfColors = colors.join(", ");
console.log(listOfColors); // "אדום, ירוק, כחול"
```

## נסוף במבט מעמיק
בעבר, כשהרי קוד לא היו נוחים ומשתמשי עומסים נמוכים, צירוף מחרוזות הוא פעולה שנראתה יקרה. היום, זו פעולה יעילה ושכיחה. ישנן דרכים שונות לצרף מחרוזות ב-TypeScript כמו אופרטור `+` או template literals, שהם יותר קריאים וקלים לניהול. `Array.join()` שימושי כאשר צריך לצרף רשימה של מחרוזות עם מפריד קבוע. במקרה של הביצועים, ההבדלים כיום הם זניחים לרוב השימושים, אבל במידרג רשימות גדולות template literals יכולים להיות יותר יעילים מקטינת שימוש בזיכרון.

## ראה גם
- [MDN String Concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [TypeScript Handbook - Template Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#template-strings)
- [JavaScript Performance - Concatenation vs. Template Literals](https://medium.com/@KevinBGreene/javascript-performance-string-concatenation-vs-template-literals-eecbfde1d2d3)
