---
title:    "Gleam: קריאת ארגומנטים של שורת פקודה"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## למה

למה כדאי לקרוא על ארגומנטים של שורת פקודה בגלים? במאמר זה נלמד כיצד לקרוא ולהשתמש בארגומנטים של שורת פקודה בגלים, ואיך זה יכול לאפשר לנו ליצור תכניות מעניינות ויעילות.

## איך לעשות זאת

כדי לקרוא ארגומנטים של שורת פקודה בגלים, ניתן להשתמש בפונקציית `Command.args()` ולשמור את התוצאה בפרמטר מסוג `Command.Args`. לדוגמה:

```Gleam
fn main(_) {
  let args = Command.args()
  case args {
    [] -> println("No arguments passed")
    [first] -> println("The first argument is: ", first)
    [first, second, third] -> println("Multiple arguments passed: ", first, ", ", second, ", ", third)
  }
}
```

כאשר מפעילים את התכנית עם הארגומנטים "Hello", "שלום" ו"こんにちは", הפלט יהיה:

```
Multiple arguments passed: Hello, שלום, こんにちは
```

## לחקור יותר

ניתן לקרוא ערכים מסוימים מתוך הארגומנטים של שורת הפקודה באמצעות פונקציית `Command.value()`. ניתן גם להעביר לפונקציה זו את הסוג המתאים של הערך שאנחנו מצפים לקבל. לדוגמה, אם נרצה לקרוא ערך מספרי מתוך הארגומנט השני, נוכל לעשות כך:

```Gleam
fn main(_) {
  let second_arg = Command.value(1, Type.Integer)
  case second_arg {
    Ok(number) -> println("The second argument is an integer: ", number)
    Error(_) -> panic("The second argument is not an integer")
  }
}
```

כעת נוכל לעבור על רשימת הארגומנטים ולכוף אם אירעה שגיאה בקריאת ארגומנטים לאחריהם. ניתן למצוא מידע נוסף על כל הפונקציות הנתמכות בפלטפורמה של גלים במפרט של הפונקציות של שפת גלים [כאן] (https://gleam.run/library/gleam/Command).

## ראו גם

- [Official Gleam Docs] (https://gleam.run/)
- [Command Line Arguments in Gleam] (https://gleam.run/library/gleam/Command)