---
title:                "חילוץ תת-מחרוזות"
html_title:           "Elixir: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## מדוע

התכנית Elixir מאפשרת לנו לחלץ חלקים מתוך מחרוזות טקסט בקלות ובמהירות. זה יכול להיות שימושי לתהליך העיבוד טקסט או פשוט להציג את המידע שאנחנו רוצים.

## איך לעשות

הנה דוגמא פשוטה של איך לחלץ מחרוזת מתוך מחרוזת יותר גדולה בשפת Elixir:

```Elixir
str = "מה נשמע?"
substring = String.slice(str, 4..7)
IO.puts(substring)
```

### פלט
נשמע

## מפנק מעמיקים

חליצת מחרוזות נמצאת בתוך המודול `String` בתכנית Elixir. קיימות מספר פונקציות שיכולות לעזור לנו לחלץ חלקים שונים מתוך מחרוזות, כולל `String.slice`, `String.substring` ו- `String.split`.

לדוגמה, הפקודה הבאה תחלץ את המילה האחרונה מתוך מחרוזת באמצעות האופרטור `++`:

```Elixir
str1 = "שלום מה שלומך?"
str2 = "נשמע יום טוב"

last_word = String.split(str1, " ") ++ String.split(str2, " ")
IO.inspect(last_word)
```

### פלט
["שלומך?", "טוב"]

## ראו גם

אם אתם מעוניינים לגלות עוד על חליצת מחרוזות בתכנית Elixir, תוכלו לבדוק את המקורות הבאים:

- [מדריך רשמי של Elixir לחליצת מחרוזות](https://elixir-lang.org/getting-started/string-patterns.html#slices-and-substrings)
- [המאמר "מתחילים עם Elixir" על חליצת מחרוזות](https://medium.com/swlh/beginners-guide-to-elixir-part-7-strings-301e5d829562)
- [תיעוד API של המודול String עבור Elixir](https://hexdocs.pm/elixir/String.html)

אתם יכולים גם למצוא עוד מאמרים וסרטוני לימוד באינטרנט על הנושא זה כדי להרחיב את הידע שלכם וליהנות מאפשרויות החליצה הרבות של התכנית Elixir.