---
title:    "Gleam: מחיקת תווים התואמים לתבנית"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה
מעקב אחרי תבנית מסוימת של תווים ומחיקתם עלול להיות נחמד כאשר אתה עובד עם מחרוזות ארוכות ובא לך להסיר את כל התווים שמתאימים לתבנית זו בפעם אחת.

## איך לעשות זאת
```Gleam
fn main() {
  let string = "Hello, world!";
  let new_string = string.replace("o", "");
  io.print(new_string); // Prints "Hell, wrld!"
}
```

דוגמאות קוד ופלט בתוך בלוקי קוד "```Gleam ... ```".

## מעמקים
מחיקת תווים מתוך מחרוזת ניתן לעשות במספר דרכים שונות. לדוגמה, ניתן להשתמש בפונקציות בניהות ב-Gleam כמו `IO.String.replace` או להשתמש בתבנית הרגולרית `Regex.replace` עבור תבניות מורכבות יותר. כמו כן, ישנן פתרונות נפוצים נוספים למחיקת תווים באמצעות הצבת תנאים ולולאות.

## ראו גם
- פונקציות בניית ב-Gleam: https://gleam.run/getting-started/functions/
- דוגמאות לפונקציות מאתגרות ב-Gleam: https://gleam.run/learn/getting-fancy/