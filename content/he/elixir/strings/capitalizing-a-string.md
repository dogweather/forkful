---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:16.679680-07:00
description: "\u05D4\u05D5\u05E4\u05DB\u05D9\u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05DB\u05D0\u05E9\u05E8\
  \ \u05DE\u05DE\u05D9\u05E8\u05D9\u05DD \u05D0\u05EA \u05D4\u05D0\u05D5\u05EA \u05D4\
  \u05E8\u05D0\u05E9\u05D5\u05E0\u05D4 \u05E9\u05DC \u05D4\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4, \u05EA\u05D5\
  \u05DA \u05D4\u05D1\u05D8\u05D7\u05D4 \u05E9\u05E9\u05D0\u05E8 \u05D4\u05D0\u05D5\
  \u05EA\u05D9\u05D5\u05EA \u05D9\u05D4\u05D9\u05D5 \u05D1\u05D0\u05D5\u05EA \u05E7\
  \u05D8\u05E0\u05D4. \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05E0\u05D7\u05D5\
  \u05E6\u05D4 \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\
  \u05EA \u05DC\u05E2\u05D9\u05E6\u05D5\u05D1 \u05E7\u05DC\u05D8\u2026"
lastmod: '2024-03-13T22:44:38.745830-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D5\u05E4\u05DB\u05D9\u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05DB\u05D0\u05E9\u05E8\
  \ \u05DE\u05DE\u05D9\u05E8\u05D9\u05DD \u05D0\u05EA \u05D4\u05D0\u05D5\u05EA \u05D4\
  \u05E8\u05D0\u05E9\u05D5\u05E0\u05D4 \u05E9\u05DC \u05D4\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4, \u05EA\u05D5\
  \u05DA \u05D4\u05D1\u05D8\u05D7\u05D4 \u05E9\u05E9\u05D0\u05E8 \u05D4\u05D0\u05D5\
  \u05EA\u05D9\u05D5\u05EA \u05D9\u05D4\u05D9\u05D5 \u05D1\u05D0\u05D5\u05EA \u05E7\
  \u05D8\u05E0\u05D4. \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05E0\u05D7\u05D5\
  \u05E6\u05D4 \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\
  \u05EA \u05DC\u05E2\u05D9\u05E6\u05D5\u05D1 \u05E7\u05DC\u05D8\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
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
