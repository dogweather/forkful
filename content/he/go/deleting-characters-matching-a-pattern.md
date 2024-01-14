---
title:                "Go: מחיקת תווים התואמים לתבנית"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

**למה:**

מחיקת תווים המתאימים לתבנית היא כלי חשוב בתוכנות כתיבה בשפת גו וניתן להשתמש בה במגוון מצבים. ניתן להשתמש בכלי זה כדי לצמצם קוד ולהפוך אותו לקריא יותר עבור המשתמשים.

**כיצד לעשות:**

שפת גו מציעה מגוון אפשרויות למחיקת תווים המתאימים לתבנית. הנה כמה דוגמאות של קוד ותוצאות:

```Go
// מחיקת תווים המתאימים לתבנית בתוך מחרוזת
str := "abcd123"
newStr := strings.ReplaceAll(str, "[a-z]", "")
// newStr = "123"

// מחיקת תווים המתאימים לתבנית בתוך בייטים
bytes := []byte{97, 98, 99, 100, 49, 50, 51}
newBytes := bytes.ReplaceAll(bytes, [97-122], []byte{})
// newBytes = []byte{49, 50, 51}
```

**עמוק יותר:**

ניתן להשתמש בכיווןים שונים כדי למחוק תווים המתאימים לתבנית. דרך אחת היא להשתמש בפונקציות כמו `strings.ReplaceAll` או `bytes.ReplaceAll` כפי שניתן לראות בדוגמאות למעלה. ניתן גם להשתמש בפונקציות כמו `strings.Map` ולייצר פונקציה מתאימה שמחליפה או מוחקת תווים בתבנית מסוימת. כמו כן, ניתן להשתמש בביטויים רגולריים כדי למצוא ולמחוק תווים בצורה מתקדמת יותר.

**ראה גם:**

- [פונקציות ReplaceAll ו- Map בחבילת strings של גו](https://golang.org/pkg/strings/)
- [פונקציות ReplaceAll ו- Map בחבילת bytes של גו](https://golang.org/pkg/bytes/)
- [ביטויים רגולריים בגו](https://golang.org/pkg/regexp/)