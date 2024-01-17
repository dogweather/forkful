---
title:                "מחיקת תווים שמתאימים לתבנית"
html_title:           "Kotlin: מחיקת תווים שמתאימים לתבנית"
simple_title:         "מחיקת תווים שמתאימים לתבנית"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים התואמים לתבנית היא תהליך בו משתמשים מפתחי תוכנה כדי להסיר תווים מסוימים תוך התאמה לתבנית מסוימת. הסיבה שמתכנתים מבצעים פעולת זו היא כי זה מאפשר להם לנקות את הנתונים שלהם ולתקן תקלות שאינן רצויות.

## איך לעשות:
משתמשים בלולאת `while` כדי לעבור דרך כל התווים במחרוזת ומשתמשים במתודת `replace` כדי להחליף את התווים שלא מתאימים לתבנית עם תווים ריקים. להלן נתונים נכונים ופלט דוגמא ב-Kotlin:

```
val string = "1 Hello! 2 Welcome to Kotlin! 3"
val pattern = Regex("""[0-9]""")
val output = string.replace(pattern, "")
println(output) // Hello! Welcome to Kotlin!
```

## מעמקים:
בעבר, כאשר תייר אמרסון המציא את שפת תבניות, לא היו לו אפשרויות לבצע פעולת מחיקת תווים כפי שהן קיימות ביום זה. עם התפתחות הטכנולוגיות, מפתחי תוכנה יכולים להשתמש במספר כלים נוספים כדי לנקות נתונים ולתקן תקלות בתוכניתים שלהם. כיום, ישנם עשרות צורות שונות לבצע פעולת מחיקת תווים תוך שימוש בשפות תכנות שונות.

## ראה גם:
למידע נוסף על מחיקת תווים התואמים לתבנית בשפת תכנות Kotlin, ניתן לעיין במסמך הרשמי: https://kotlinlang.org/docs/reference/regexp.html