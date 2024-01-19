---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המציאת אורך של מחרוזת בעצם מתייחסת למספר התווים שבה. מתכנתים נדרשים לבצע זאת מגוון סיבות, כולל שליטה על תהליכים ברמת אורך נתונים או בדיקת תקינות קלט.

## איך ל:
אנו משתמשים בפונקציה `String.length()` כדי למצוא את האורך של מחרוזת ב Elixir.

```elixir
iex> string = "אני מחרוזת!"
iex> String.length(string)
11
```

אז זה תצוגה של מחרוזת בעברית, ואנו מציגים את האורך שלה - 11.

## הצלילה המעמיקה
אם אנחנו רוצים לדעת קצת היסטוריה, `String.length()` באמת נקודת המפנה של אחת הפונקציות האינטואיטיביות ביותר ב Elixir. כשאנו מדברים על אלטרנטיבות, אנו שום פעם לא מתקנתים את הגלגל, אפשר להשתמש בגרף אוניקוד של אליקסיר. סימני האוניקוד מייצגים מחרוזת כסדרה של פוינטים של מחרוזות.

```elixir
iex> string = "אני מחרוזת!"
iex> String.graphemes(string) |> Enum.count()
11
```

בעת חיפוש אחר פרטי המימוש של `String.length()`, אפשר לראות בקוד המקור שנעשה שימוש בפה אחד סריקת מחרוזת מפונקציה העזר הפנימית `:unicode_util.string_length`.

## ראו גם
[תיעוד Elixir למחרוזות](https://hexdocs.pm/elixir/String.html)
[הפוסט של ג'ו ארמסטרונג על אוניקוד באריקס](https://joearms.github.io/2015/01/31/unicode-is-kicking-my-butt.html)