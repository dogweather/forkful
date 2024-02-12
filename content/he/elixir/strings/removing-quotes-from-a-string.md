---
title:                "הסרת מרכאות ממחרוזת"
aliases: - /he/elixir/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:06.651952-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/removing-quotes-from-a-string.md"
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
