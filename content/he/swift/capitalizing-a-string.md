---
title:    "Swift: שימוש גדל במחרוזת"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#למה

 כאשר אנו מתכנתים בשפת סוויפט, יתכן שנתקל במצבים שבהם נצטרך להפוך את הטקסט שהמשתמש הכניס למחרוזת לכתיב גדול. כדי לעזור לך להבין שלמה את הצורך לשנות את מחרוזת בכתיב גדול, אנו רוצים לחלק את המידע באמצעות מאמר זה. 

#כיצד לעשות זאת 

כדי להמיר מחרוזת לכתיב גדול בשפת סוויפט, ניתן להשתמש בפונקציה `uppercased()` . ניתן גם להשתמש בפונקציה `capitalized()` כדי להמיר את ראשות המילים במחרוזת לכתיב גדול. הנה כמה דוגמאות קוד כדי להבין איך להשתמש בפונקציות הללו:

```Swift 
let name = "דניאל"

print(name.uppercased())
// Output: דניאל

print(name.capitalized())
// Output: דניאל
```

בנוסף, אם נרצה להמיר רק את האות הראשונה של המחרוזת לגדולה, ניתן להשתמש בפונקציה `prefix()` כך:

```Swift 
let name = "daniel"

let firstLetter = name.prefix(1).uppercased()
let restOfName = name.dropFirst()
let capitalized = firstLetter + restOfName

print(capitalized)
// Output: Daniel
```

#עומק נעמד 

בשפת סוויפט, יש כמה פונקציות שיכולות לעזור לנו להמיר מחרוזות לכתיב גדול. בנוסף, אנו ניתנים את האופציה להמיר את המחרוזת לכתיב גדול בצורה של כתיב גדול המחזיר מחרוזת חדשה, או לשנות את המחרוזת הקיימת בצורה ישירה ע"י שימוש בפונקציות `uppercased()` ו- `capitalized()` . עם זאת, חשוב לזכור שהפונקציות הללו אינן משנות את המחרוזת המקורית, אלא יוצרות מחרוזות חדשות.

#ראה גם 

- [מדריך מלא על שתי הפונקציות `uppercased()` ו- `capitalized()` ](https://developer.apple.com/documentation/swift/string/