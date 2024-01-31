---
title:                "מחיקת תווים התואמים לתבנית"
date:                  2024-01-20T17:44:10.850158-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?

ב TypeScript, למחוק תווים התואמים תבנית זה לעבוד עם קטעי טקסט ולהסיר את מה שאיננו רצוי. מתכנתים עושים זאת לנקות קלט, לשפר מבנה נתונים, או להתאים טקסט לשימוש מסוים.

## איך לעשות:

כדי למחוק תווים שתואמים תבנית ב-TypeScript, אתה יכול להשתמש בביטויים רגולריים עם המתודה `replace`. דוגמה:

```typescript
const cleanString = (str: string, pattern: RegExp): string => {
  return str.replace(pattern, '');
};

const exampleString = 'Hey! How are you doing today?';
const cleanedString = cleanString(exampleString, /[?!]/g); // הסרת סימני שאלה וקריאה

console.log(cleanedString); // הדפסת התוצאה
```

פלט דוגמה:

```
Hey How are you doing today
```

## נפילה לעומק:

ביטויים רגולריים (Regex) הם כלי חזק בכל שפת תכנות, וכבר קיימים מאז שנות ה-60. ב-TypeScript, שהיא הרחבה קטנועית של JavaScript, השימוש ב-Regex דומה מאוד. מלבד `replace`, יש גם שיטות אחרות כמו `match` ו-`search` שמאפשרות עבודה עם תבניות. ב-Symbols חדשניים ב-RegExp יכולים לשפר את הביצועים בהתאם לפעולות שביצעת – למשל, שימוש בסימן `g` לחיפוש גלובלי.

האם יש חלופות ל-RegExp? כן, דרך אחרת היא להשתמש במתודות מחרוזת כמו `split` ו-`join`. זה יכול להיות אפקטיבי אם אתה צריך להסיר תווים פשוטים, אבל במקרים מורכבים, Regex הוא כלי עוף.

## ראה גם:

- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [RegExp שימושים וטיפים](https://regexr.com/)
