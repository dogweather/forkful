---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:05.237996-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D0\u05DC\u05D9\
  \u05E7\u05E1\u05D9\u05E8 \u05DE\u05E9\u05EA\u05DE\u05E9\u05EA \u05D1\u05DE\u05D5\
  \u05D3\u05D5\u05DC `Regex`, \u05E9\u05DE\u05E0\u05E6\u05DC \u05D0\u05EA \u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05EA \u05D4-regex \u05E9\u05DC Erlang, \u05DC\u05E6\u05D5\u05E8\
  \u05DA \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA regex.\
  \ \u05D4\u05E0\u05D4 \u05DB\u05DE\u05D4 \u05E9\u05D9\u05DE\u05D5\u05E9\u05D9\u05DD\
  \ \u05D1\u05E1\u05D9\u05E1\u05D9\u05D9\u05DD."
lastmod: '2024-03-13T22:44:38.756844-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05DC\u05D9\u05E7\u05E1\u05D9\u05E8 \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05EA \u05D1\u05DE\u05D5\u05D3\u05D5\u05DC `Regex`, \u05E9\u05DE\u05E0\u05E6\u05DC\
  \ \u05D0\u05EA \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4-regex \u05E9\u05DC Erlang,\
  \ \u05DC\u05E6\u05D5\u05E8\u05DA \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\
  \u05DC\u05D5\u05EA regex."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך לעשות:
אליקסיר משתמשת במודול `Regex`, שמנצל את ספריית ה-regex של Erlang, לצורך ביצוע פעולות regex. הנה כמה שימושים בסיסיים:

```elixir
# התאמת תבנית - מחזירה את ההתאמה הראשונה
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # פלט: ["hello"]

# מציאת כל ההתאמות
all_matches = Regex.scan(~r/\d/, "יש 2 תפוחים ו-5 תפוזים.")
IO.inspect(all_matches) # פלט: [["2"], ["5"]]

# החלפת חלקים במחרוזת
replaced_string = Regex.replace(~r/\s+/, "אליקסיר זה כיף", "_")
IO.inspect(replaced_string) # פלט: "אליקסיר_זה_כיף"
```

לתבניות ופונקציונליויות יותר מורכבות, כדאי לשקול שימוש בספריות צד שלישי, אך למשימות הקוריים הרבות של התאמת מחרוזות ותבניות, המודול `Regex` המובנה של אליקסיר חזק מאוד.

לביצוע התאמה ללא תלות ברישיות, יש להשתמש באופציה `i`:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # פלט: ["Hello"]
```

ביטויי regex ניתנים לקומפילציה מראש לצורך יעילות כאשר משתמשים בהם מספר פעמים:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # פלט: ["hello"]
```

אליקסיר תומכת גם בלכידות בשמות, שיכולות להיות שימושיות מאוד לחילוץ חלקים מסוימים מתוך מחרוזת תוך שמירה על קוד נקי וקריא:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # פלט: %{"year" => "2023", "month" => "04", "day" => "15"}
```

סקירה זו מדגישה את הקלות שבה אליקסיר מתמודדת עם ביטויים רגולריים, ומאפשרת טכניקות חזקות של טיפול במחרוזות וחילוץ נתונים.
