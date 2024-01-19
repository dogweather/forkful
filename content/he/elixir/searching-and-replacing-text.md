---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
‏חיפוש והחלפה של טקסט הם פעולות שבהן המחשב מצא את כל תכני המחרוזת המסוימת במחרוזת אחרת והחליף אותם במחרוזת שלישית. מתכנתים מבצעים את זה בעיקר ללמד את המחשב לדעת מתי לשנות קוד כדי להשיג תנאים מסוימים.

## איך לעשות:
ב- Elixir, אתה יכול להשתמש בפונקציה `String.replace/3` לחיפוש והחלפת טקסט. דוגמא:

```Elixir 
original_text = "שלום, עולם"
IO.puts String.replace(original_text, "עולם", "Elixir")
```

הפלט של הקוד הזה יהיה:

```Elixir 
"שלום, Elixir"
```
אפשר גם להשתמש בקטע ציור RegEx:

```Elixir 
IO.puts String.replace("גרשם in the יער", ~r/\b(גרשם|יער)\b/, "\\0דן")
```

## צלילה עמוקה:
חיפוש והחלפה של טקסט הוא אבן פינה בתכנות מאז ימי עודד התכנות המוקדמים. במרבית השפות, קיימים אלטרנטיבות לפונקציה `replace` של Elixir, כמו `replacen` ב- JavaScript או `replace_all` ב- Python. אין להימנע מהשימוש בהם רק אם אתה מעוניין לבצע החלפה מרובה. 

אפשר גם להשתמש ב- RegEx אם אתה רוצה לעבוד עם מחרוזות של תנאים משונים, אך תשקול להיות מנומס עם השימוש בו כיוון שהוסףת יותר עומס על המערכת.

## ראה גם:
1. [מדריך המתחילים של Elixir](https://elixir-lang.org/getting-started/basic-types.html)
2. [מידע נוסף על String.replace](https://hexdocs.pm/elixir/String.html#replace/3)
3. [מידע נוסף על RegEx ב- Elixir](https://elixir-lang.org/getting-started/sigils.html#regular-expressions)