---
title:                "קינון מחרוזות"
html_title:           "Elixir: קינון מחרוזות"
simple_title:         "קינון מחרוזות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

כשאת/ה כותב/ת קוד ב-Elixir, ייתכן שתזדקק/י ליצירת מחרוזות מרוכבות ממחרוזות קיימות. כולם יכולים להיות "לכוננות" מחרוזת נקנה קיימת באמצעות פעולות כגון הוספת, מחיקה או כיבוץ. ליצור מחרוזות כאלו היא דרך נוחה ויעילה לתזמן את הקוד הנתון.

## כיצד

 ```elixir
str1 = "שלום"
str2 = "עולם!"

# הוספת שני מחרוזות
result = str1 <> str2
IO.puts result

# החלפת נקנה מחרוזת
new_str = "להתראות!" <> result
IO.puts new_str

# כיבוץ נקנה מחרוזת
combined = ";) " <> new_str <> " " <> result <> " :("
IO.puts combined

# ייצוג מקוצר
IO.puts "Hello " <> str2
```

```
שלוםעולם!
להתראות!שלוםעולם!
;) להתראות!שלוםעולם! :(
Hello עולם!
```

## צליל מעמיק

כשניצור מחרוזת באמצעות הפעולה <> (כמו שעשינו בדוגמה), הדבר הראשון שאלין הוא האם אחד המשתנים הוא נוקנו. אם אחד מהם אינו את המחרוזת, Elixir יכולה להראות שגיאה. כדי להתחשב בדפוסים שונים, ניתן להשתמש בפעולה של טרוקו הכוללת מספרים. למעשה, מצטמצם שימוש זה במחרוזות מצדדת ומאפשר להפעיל את הקוד במגוון מכשילים.

## ראה גם

- [דוקומנטציה רשמית על פעולת מגוון](https://hexdocs.pm/elixir/Kernel.SpecialForms.Html)
- [למידה בחינוך אינטראקטיבי](https://elixirschool.com/en/lessons/basics/strings/)
- [בניית מחרוזות ב-Elixir עם Interpolation](https://www.poeticoding.com/interpolate-strings-in-elixir/)