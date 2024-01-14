---
title:    "TypeScript: מחיקת תווים התואמים לתבנית"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## למה

לעולם אנו מחפשים דרכים להשתמש בפונקציות שונות כדי לקבל את התוצאות שאנו רוצים. מפעיל מחשבים נלחם מיום יום עם כמויות עצומות של נתונים ולפעמים נדרש למחוק תווים מסוימים לפני שאנו יכולים לעבוד עם הנתונים. במקרים רבים, ניתן למחוק תווים באמצעות תבניות יחידות כדי להפוך את התהליך הזה לאוטומטי ויעיל יותר. בכתבה זו נשתמש בטיפול בתווים המתאימים לתבנית בטיפול בקודים בטיפוסקריפט.

## איך לעשות

הנה דוגמאות לקודים ופלטים לטיפוסקריפט בתוך בלוקי קוד "```TypeScript ... ```" כדי לדגום איך למחוק תווים המתאימים לתבנית:

קוד קוד המוחק את כל האותיות הקטנות מהמחרוזת:
```TypeScript
let str = "Hello World";
let newStr = str.replace(/[a-z]/g,"");
console.log(newStr); // Output: H W
```

קוד קוד המוחק את כל הספרות מהמחרוזת:
```TypeScript
let str = "123abc456";
let newStr = str.replace(/[0-9]/g,"");
console.log(newStr); // Output: abc
```

קוד קוד המוחק את כל התווים המתאימים לתבנית מכל המחרוזות במערך:
```TypeScript
let arr = ["apple", "banana", "carrot"];
let newArr = arr.map(str => str.replace(/[a-z]/g,""));
console.log(newArr); // Output: ["pp", "bn", "crrt"]
```

כאן ניתן לראות שאנו משתמשים בתבנית יחידה ([a-z] למחרוזת הרגילה ולמערך ניתן להשתמש בפונקציית מפה כדי לעבור על כל המחרוזות ולהחליף את התווים המתאימים לתבנית.

## כיוון מעמיק

ניתן להתרחק יותר בעבודה עם תבניות יחידות בטיפוסקריפט ולכלול אותן בתהליך מורכב יותר. הנה כמה ד