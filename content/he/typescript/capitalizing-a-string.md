---
title:                "הבהרת שרשרת"
html_title:           "TypeScript: הבהרת שרשרת"
simple_title:         "הבהרת שרשרת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
מידי פעם בזמן כשאתה כותב קוד בטיפוסקריפט, עלול להיות לך צורך להדפיס טקסט עם האות הראשונה בגודל גדול. במקרים כאלה, ייתכן ותרצה להחזיר את הטקסט עם אות הראשונה באות גדולה כדי להדגיש יותר את המידע החשוב.

## איך לעשות זאת
ישנם מספר דרכים להגדיל את האות הראשונה של טקסט בטיפוסקריפט. נהדיר פונקציה פשוטה שתקבל מחרוזת ותחזיר את אותה מחרוזת עם האות הראשונה באות גדולה. הנה דוגמא קודם כל:

```typescript
function capitalizeString(str: string) {
  return str[0].toUpperCase() + str.slice(1);
}

console.log(capitalizeString("hello")); // Output: Hello
```

ניתן גם להשתמש בפעולת החלפה (replace) כדי להחליף את האות הראשונה באות גדולה ולשמור על שאר המחרוזת:

```typescript
function capitalizeString(str: string) {
  return str.replace(/^\w/, c => c.toUpperCase());
}

console.log(capitalizeString("world")); // Output: World
```

קל לשכור שאפשרויות רבות נוספות לכתיבת פונקציות כדי להגדיל את הטקסט, מה שמאפשר לנו להתאים ולשנות את הקוד בהתאם לצורך שלנו.

## מה זה אומר בעצם להגדיל את הטקסט?
בדרך כלל, אנחנו משתמשים בפעולה כדי להכניס דגש על מילה מסוימת בטקסט. האות הראשונה בגודל גדול מסמלת חשיבה והדגשה על המלה הזאת והופכת אותה לחלק חשוב יותר מהטקסט הסביבתי.

## ראית גם
- [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [CSS :first-letter pseudo-element](https://developer.mozilla.org/en-US/docs/Web/CSS/::first-letter)