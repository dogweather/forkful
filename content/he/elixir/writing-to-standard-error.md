---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה כתיבה לשגיאה סטנדרטית ולמה זה נחוץ? כתיבה לשגיאה סטנדרטית (stderr) מאפשרת להפריד מידע על שגיאות מהפלט הרגיל של תוכנית. פרוגרמרים עושים את זה כדי לתעד בעיות ולעזור באבחון תקלות בלי לבלבל את הפלט העיקרי.

## How to:
הנה דוגמא פשוטה לכתיבה לstderr באליקסיר:

```elixir
# כתיבה לstdout
IO.puts("Hello, stdout!")

# כתיבה לstderr
:io.put_chars(:standard_error, "Error: Something went wrong!\n")
```

פלט לדוגמא:
```
Hello, stdout!
Error: Something went wrong!
```

כאשר אתה מפעיל את הקוד הזה, תראה את הודעת השגיאה בצבע אחר או בחלון אחר, בהתאם לסביבת העבודה שלך.

## Deep Dive:
במערכות יוניקס ודומות, stderr היא אחת משלוש ערוצי תקשורת מובנים לתוכנית (לצד stdout וstdin). כתיבה לstderr מאוד נפוצה בסקריפטים ופקודות של. בשפת אליקסיר, פונקצית :io.put_chars היא דרך נפוצה לכתוב לstderr, אך ישנן גם אלטרנטיבות כגון השימוש במודול Logger לתיעוד מקיף יותר.

## See Also:
- [Elixir IO Module](https://hexdocs.pm/elixir/IO.html)
- [Elixir Logger Module](https://hexdocs.pm/logger/Logger.html)
- [Understanding Shell Script's idiom: 2>&1](https://www.tldp.org/LDP/abs/html/io-redirection.html)
