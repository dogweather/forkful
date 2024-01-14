---
title:    "Elixir: חילוץ תת-מחרוזות"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

מדוע בכלל כדאי למישהו לעסוק בהוצאת תת-מחרוזות? התשובה היא בכלל לא מסובכת. הוצאת תת-מחרוזות היא כלי חשוב בתכנות ב-Elixir כי ניתן להשתמש בו כדי לעבוד עם מחרוזות מסובכות ולאפשר יכולות רבות יותר.

## כיצד לעשות את זה

כדי להשתמש בתת-מחרוזות ב-Elixir, ניתן להשתמש בפונקציות כמו `String.slice/2` ו- `String.slice/3`. להלן כמה דוגמאות של קוד ופלט, עם ההצהרות `import String` הנוספות כדי לפשל את הקוד:

```elixir
iex> import String
iex> input_string = "לפני כמה זמן צפינו סרט יפה"
iex> String.slice(input_string, 8, 14)
"כמה זמן"
```

בנוסף, ניתן להשתמש גם באופרטור המובנה `..` כדי להרחיב על המחרוזות הפשוטות:

```elixir
iex> input_string = "הספר הכי יפה שקראתי השנה"
iex> input_string[4..-7]
"יפה שקראתי"
```

כדי להחיל תת-תבנית למחרוזת, ניתן להשתמש בפונקציות כמו `String.replace/4` ו- `String.replace_leading/4`, כמו בדוגמאות הבאות:

```elixir
iex> input_string = "שלום הוראת משתנה היא $HAMLET"
iex> String.replace(input_string, "$HAMLET", "המלט")
"שלום הוראת משתנה היא המלט"
```

```elixir
iex> input_string = "פנטזיות עשוים עם מורה שליו"
iex> String.replace_leading(input_string, "עשוים", "יוצרים")
"פנטזיות יוצרים עם מורה שליו"
```

## חפירה עמוקה

הוצאת תת-מחרוזות היא פעולה נפוצה יותר ממה שנראה. בכלל, כאשר מתכנתים עובדים עם מחרוזות ב-Elixir, הם משתמשים בתת-מחרוזות כדי לבצע ארגומנטים ולהעביר פרסומות מעניינות לקונסולת הפק