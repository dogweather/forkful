---
title:                "הגדלת אותיות במחרוזת"
aliases: - /he/fish-shell/capitalizing-a-string.md
date:                  2024-02-03T19:05:41.811360-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

הפיכת מחרוזת לראשית גדולה משמעה שינוי שם האות הראשונה לאות גדולה ושאר המחרוזת לאותיות קטנות. זהו משימה נפוצה בעיבוד טקסט, נרמול קלט משתמש ועיצוב נתונים כדי להבטיח עקביות או לעמוד בקריטריונים ספציפיים של עיצוב.

## איך לעשות:

ב-Fish Shell, ניתן לעבד מחרוזות ישירות עם פונקציות מובנות, מבלי צורך בכלים חיצוניים או ספריות. לצורך הפיכת מחרוזת לראשית גדולה, ניתן לשלב את פקודת `string` עם תת-פקודות.

```fish
# מחרוזת לדוגמא
set sample_string "hello world"

# הפיכת האות הראשונה לראשית גדולה
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

פלט:
```
Hello world
```

לצורך תרחישים שדורשים הפיכת מספר מילים במחרוזת לאות ראשית גדולה (לדוגמא, המרת "hello world" ל-"Hello World"), יש לנהל לולאה על כל מילה, תוך החלת לוגיקת ההפיכה לראשית גדולה על כל אחת:

```fish
# משפט לדוגמא
set sentence "hello fish shell programming"

# הפיכת כל מילה לראשית גדולה
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# שילוב המילים המופכות לראשיות גדולות מחדש
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

פלט:
```
Hello Fish Shell Programming
```

שימו לב ש-Fish Shell אינו מציע ישירות גישה של פקודה יחידה להפיכת משפטים באופן מלא כמו שחלק משפות התכנות עושות עם שיטות מחרוזת שלהן. לכן, שילוב `string split`,` string sub`, `string upper`, ואז שילוב מחדש, מייצג גישה אידיומטית ב-Fish Shell להשגת זאת.
