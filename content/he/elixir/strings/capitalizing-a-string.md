---
title:                "הגדלת אותיות במחרוזת"
aliases:
- /he/elixir/capitalizing-a-string.md
date:                  2024-02-03T19:05:16.679680-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

הופכים מחרוזת לאות ראשית כאשר ממירים את האות הראשונה של המחרוזת לאות גדולה, תוך הבטחה ששאר האותיות יהיו באות קטנה. פעולה זו נחוצה לעיתים קרובות לעיצוב קלט משתמש או הצגת טקסט בממשקי משתמש, שם עקביות וניתנות לקריאה חשובות.

## איך לעשות:

Elixir מספקת דרך ישירה להפוך מחרוזות לאות ראשית באמצעות הפונקציות המובנות שלה ללא הצורך בספריות צד שלישי. הנה דוגמה פשוטה:

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

פלט:

```
Elixir programming
```

למקרים בהם נדרש שליטה רבה יותר או לוגיקת הופכת לאות ראשית מורכבת יותר, תוכלו לשלב פונקציות String שונות. למשל, אם אתם רוצים להפוך כל מילה במשפט לאות ראשית, תוכלו לפצל את המשפט למילים, להפוך כל אחת לאות ראשית, ולאחר מכן לחבר אותם חזרה יחד:

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

פלט:

```
Elixir Is Fun
```

למרות שספריית הסטנדרט של Elixir מספקת מענה לרוב הצרכים, למניפולציה טקסטואלית עדינה יותר, כולל הופכן מורכבת של מחרוזות, ייתכן ותחקרו ספריות צד שלישי כמו Cldr לבינלאומיות, המספקת התנהגויות הופכן תלויות אזור.
