---
date: 2024-01-26 03:40:06.651952-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4 \u05DC\
  \u05D4\u05E9\u05D9\u05DC \u05D0\u05EA \u05D4\u05DE\u05E2\u05D8\u05E4\u05D5\u05EA\
  \ \u05D4\u05E0\u05D5\u05E1\u05E4\u05D5\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\
  \u05DC \u05D0\u05EA \u05D4\u05D8\u05E7\u05E1\u05D8 \u05D4\u05E0\u05E7\u05D9 \u05DE\
  \u05D1\u05E4\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E1\u05E0\
  \u05DF \u05E7\u05DC\u05D8\u05D9\u05DD, \u05DC\u05D4\u05D9\u05DE\u05E0\u05E2 \u05DE\
  \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA, \u05D5\u05DC\u05D4\u05DB\u05D9\u05DF \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3\u2026"
lastmod: '2024-02-25T18:49:37.066606-07:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4 \u05DC\u05D4\
  \u05E9\u05D9\u05DC \u05D0\u05EA \u05D4\u05DE\u05E2\u05D8\u05E4\u05D5\u05EA \u05D4\
  \u05E0\u05D5\u05E1\u05E4\u05D5\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC\
  \ \u05D0\u05EA \u05D4\u05D8\u05E7\u05E1\u05D8 \u05D4\u05E0\u05E7\u05D9 \u05DE\u05D1\
  \u05E4\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E1\u05E0\u05DF\
  \ \u05E7\u05DC\u05D8\u05D9\u05DD, \u05DC\u05D4\u05D9\u05DE\u05E0\u05E2 \u05DE\u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA, \u05D5\u05DC\u05D4\u05DB\u05D9\u05DF \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3\u2026"
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת מרכאות ממחרוזת פירושה להשיל את המעטפות הנוספות כדי לקבל את הטקסט הנקי מבפנים. מתכנתים עושים זאת כדי לסנן קלטים, להימנע משגיאות, ולהכין נתונים לעיבוד היכן שהמרכאות הן מטרדים, לא תכונות.

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
