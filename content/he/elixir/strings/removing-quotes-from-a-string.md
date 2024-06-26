---
date: 2024-01-26 03:40:06.651952-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Elixir \u05D0\
  \u05D9\u05DF \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05DE\u05D5\u05D1\u05E0\
  \u05D9\u05EA \u05DC\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA\
  , \u05D0\u05DA \u05E7\u05DC \u05DE\u05D0\u05D5\u05D3 \u05DC\u05D9\u05E6\u05D5\u05E8\
  \ \u05D0\u05D7\u05EA \u05DE\u05E9\u05DC\u05DA \u05E2\u05DD \u05D4\u05EA\u05D0\u05DE\
  \u05EA \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05D0\u05D5 \u05E4\u05D5\u05E0\u05E7\
  \u05E6\u05D9\u05D5\u05EA \u05E9\u05DC `String`. \u05E8\u05D0\u05D4 \u05D0\u05EA\
  \ \u05D4\u05E7\u05D8\u05E2\u05D9\u05DD \u05D4\u05D0\u05DC\u05D4."
lastmod: '2024-03-13T22:44:38.753852-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Elixir \u05D0\u05D9\u05DF \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\
  \u05DB\u05D0\u05D5\u05EA, \u05D0\u05DA \u05E7\u05DC \u05DE\u05D0\u05D5\u05D3 \u05DC\
  \u05D9\u05E6\u05D5\u05E8 \u05D0\u05D7\u05EA \u05DE\u05E9\u05DC\u05DA \u05E2\u05DD\
  \ \u05D4\u05EA\u05D0\u05DE\u05EA \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05D0\u05D5\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05E9\u05DC `String`."
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

## איך לעשות:
ב-Elixir אין פונקציה מובנית להסרת מרכאות, אך קל מאוד ליצור אחת משלך עם התאמת תבניות או פונקציות של `String`. ראה את הקטעים האלה:

```elixir
# באמצעות התאמת תבניות
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# שימוש לדוגמא
unquote_string("\"שלום, עולם!\"") # => "שלום, עולם!"
unquote_string("'שלום, עולם!'")   # => "שלום, עולם!"

# באמצעות String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# שימוש לדוגמא
unquote_string("\"שלום, עולם!\"") # => "שלום, עולם!"
unquote_string("'שלום, עולם!'")   # => "שלום, עולם!"
```

הפלט לשתי השיטות יהיה:
```
"שלום, עולם!"
```

## צלילה עמוקה
בימים ההם, מרכאות במחרוזות היו שדה מוקשים - טעות בהטיפול בהן, וזה, שגיאות תחביר או חורי אבטחה. ב-Elixir, התאמת תבניות מטפלת במחרוזות שלך כמו בלוקי לגו, מאפשרת לך לפרק ולבנות מחדש בדיוק. המודול `String` האמין שלה גם מגיע נוחות ליד, מסיר מרכאות באופן גמיש עם פונקציות `trim`. האלטרנטיבות? ביטויים רגולריים יכולים להעיף מרכאות לצדדים, וספריות חיצוניות עשויות להכיל כוח זרוע נוסף אם אתה צריך יותר מסתם הסרה בסיסית.

## ראה גם
צלול עמוק יותר עם אלה:
- [מודול ה-String של Elixir](https://hexdocs.pm/elixir/String.html)
- [למד יותר על התאמת תבניות ב-Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [ביטויים רגולריים ב-Elixir (מודול Regex)](https://hexdocs.pm/elixir/Regex.html)
