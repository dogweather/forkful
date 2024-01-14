---
title:    "Elixir: חיפוש והחלפת טקסט"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

השימוש בחיפוש והחלפה של טקסט הוא כלי חשוב בכל שפת תכנות, כולל ב-Elixir. יתרון השימוש בפונקציות מובנות במקום בכתיבת קוד מחדש הוא בביטחון וביעילות משופרת.

## איך לעשות זאת

כדי לחלוף על טקסט ב-Elixir, ניתן להשתמש בפונקציה `String.replace/3`. פונקציה זו מקבלת שלושה פרמטרים: המחרוזת שבה כתוב הטקסט המקורי, הטקסט שברצונכם להחליף, והטקסט החדש שתרצו שיוחלף על ידו. לדוגמה:

```elixir
string = "שלום עולם"
new_string = String.replace(string, "עולם", "מאתנו")
```

במקרה זה, המחרוזת החדשה `new_string` תהיה "שלום מאתנו".

## חפירה מעמיקה

בנוסף לפונקציה `String.replace/3`, ישנן פונקציות נוספות לחיפוש והחלפת טקסט ב-Elixir. למשל, `String.replace_leading/3` ו-`String.replace_trailing/3` נועדו להחליף רק את הטקסט המתחיל או נגמר בטקסט המקורי. ישנן גם פונקציות נוספות להתאמה עם ביטויים רגולריים, כמו `Regex.replace/3` ו-`Regex.replace_leading/3`. כל הפונקציות הללו מכילות בעצם את הפונקציה `String.replace/3`, אך מותאמות למטרות ספציפיות.

## ראו גם

- [מדריך על השתמשות בפונקציות טקסט ב-Elixir](https://elixir-lang.org/getting-started/string-patterns-and-operations.html)
- [מדריך על שימוש בביטויים רגולריים ב-Elixir](https://elixir-lang.org/getting-started/regex.html)
- [תיעוד על הפונקציות המובנות להחלפת טקסט ב-Elixir](https://hexdocs.pm/elixir/String.html#functions-for-string-manipulation)