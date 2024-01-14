---
title:                "Go: שימוש בביטויים רגילים"
simple_title:         "שימוש בביטויים רגילים"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

# למה

בעולם התכנות ישנו עולם מלא בשמות, מספרים ותווים שונים. נסיונות למצוא את המידע הנדרש על בסיס של מחרוזות בעקבות מסורתית לא תמיד מצליחים - בוודאי אם המידע לא תקין, או שהוא מכיל שגיאות. זאת הסיבה שאנו צריכים כלי מתאים לכתיבת תבניות חיפוש וסינטקסט התאמה למחרוזות. הכי טוב היום לכתוב את התבניות על בסיס של שפת תכנות אשר מאפשרת כך.

# איך ל

נדרש עריכות תכנות בכדי להגדיר תעדפות עבור תבניות חיפוש וסינטקסט אשר טכנית יתאימו למיומנויות שלנו. אנו יכולים כמה פעמים מפעם להשתמש בפונקציות אשר מסייעות להציג את הפרטים אשר שבקורת אנו מכירים בכדי להתמודד עם מנגנים בסיסיים לחיפוש וסינטקסט, אך הבדיקה ערכים לא מתאימים. לכן, עם גושי מידע לסייע אותנו יש לכתוב את כך "Go ... ".

```Go
// Package main imports the fmt package for printing to stdout.
package main

import "fmt"

func main() {
    // Define a string variable with a text.
    str := "Hello, world!"

    // Use a regular expression to search for 'Hello' in the string.
    matched, err := regexp.MatchString("Hello", str)

    // Check the results.
    if err != nil {
        // Handle errors.
        fmt.Println("There was an error:", err)
    } else {
        // Print the result.
        fmt.Println("Is 'Hello' matched? ", matched)
    }
}
```

# לחקירה מעמיקה

כעת, שנוכל להשתמש בתבניות קבועות לסייע לנו בהתאמה לתיק מחרוזות אחרת נסתכל על פונקציות אחרון גדול יותר אשר יכולות להעזר בכתיבת שפת תכנות בחיפוש תבניות וסינטקסט. זאת הסיבה שייתכן ונצטרך ללכת לעומק נוסף כפי