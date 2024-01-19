---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?  
String interpolation היא תהליך שבו אנחנו משלבים משתנים או ביטויים בוליאניים בתוך מחרוזת. באמצעות הטכניקה הזאת, מאפשרים לנו ליצור מחרוזות גמישות ורב-תכליתיות ללא הצורך להתמקד בסדר המילים או קיצוניות טקסט.

## איך להשתמש:
ב-Elixir, נשתמש בתווים `{}` כדי לערבב משתנה או ביטוי בתוך מחרואת.

```Elixir
name = "Moshe"
IO.puts "שלום, #{name}"
# Outputs: שלום, Moshe
```

בדוגמה הזו, אנחנו משתמשים ב-interpolation כדי להוסיף את השם של Moshe למחרוזת שלנו.

## צלילה עמוקה: 
חדשות טובות - אף פעם לא אפשרי להכניס תווים מיותרים ב-interpolation של Elixir. הדקדוק שלו מופשט, כך שהוא זקוק לסוגריים מסולסלים במקרה של interpolation. פונקציות, ביטויים מתמטיים או שימוש בתנאי- אפשריים כולם! 

אבל במקרה שלא צריך להשתמש ב-interpolation, מחרוזות מאוחדות יכולות להיות אלטרנטיבה נהדרת.
```Elixir
name = "Moshe"
simple_greeting = "שלום, " <> name
IO.puts simple_greeting
# Outputs: שלום, Moshe
```
המחרוזת המאוחדת לא הכרחית למשלים של מילה, אבל היא טיפה יותר מהירה מאשר interpolation.

## ראה גם:
1. Elixir’s official guide on string interpolation: [Elixir Interpolation](https://elixir-lang.org/getting-started/io-and-the-file-system.html#iodots)
2. Elixir School’s lesson on strings: [Elixir Strings](https://elixirschool.com/en/lessons/basics/strings/)