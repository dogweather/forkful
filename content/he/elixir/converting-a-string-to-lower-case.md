---
title:                "Elixir: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# למה
במאמר זה נלמד כיצד להמיר מחרוזת לאותיות קטנות בשפת אליקסיר. זה יהיה מועיל למתכנתים שעובדים עם מחרוזות ורוצים לטפל בהן בצורה קלה ומאובטחת.

# כיצד לעשות זאת
הנה כמה דוגמאות להמיר מחרוזת לאותיות קטנות באמצעות שימוש בפונקציה `String.downcase/1`:

```Elixir
iex> String.downcase("HELLO WORLD")
"hello world"
iex> String.downcase("Elixir Programming")
"elixir programming"
```

# מעמקים
להמרת מחרוזת לאותיות קטנות יש הרבה יתרונות, כולל יצירת ייצוג יחודי ואיכותי של מחרוזת בשפת התסריט שלך וטיפול בנתונים אלגנטי יותר. כמו כן, זה מסייע למניעת טעויות כתיב ואיתור באגים במתודות אחרות שבוצעות על מחרוזות.

# ראו גם
- [הדרכות ומדריכים לאליקסיר](https://elixir-lang.org/getting-started/introduction.html)
- [מדריך למתחילים: המבוא המהיר לאליקסיר](https://elixirschool.com/he/)
- [תיעוד על מחרוזות באליקסיר](https://hexdocs.pm/elixir/String.html)