---
title:                "חיפוש והחלפת טקסט"
aliases:
- /he/elixir/searching-and-replacing-text.md
date:                  2024-01-20T17:58:08.631458-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפה של טקסט הם פעולות שבהן אנו מאתרים תבנית מסוימת של טקסט ומחליפים אותה באחרת. תכניתנים עושים זאת כדי לעדכן באופן מהיר מידע, לתקן שגיאות, או לבצע ריפקטורינג לקוד.

## איך לעשות:
```Elixir
# יצירת טקסט לדוגמא
original_text = "אהבה זה הכל, אהבה זה פה ושם."

# חיפוש והחלפה
new_text = String.replace(original_text, "אהבה", "שלום")

IO.puts new_text
# פלט: שלום זה הכל, שלום זה פה ושם.

# חיפוש והחלפה עם רגקס
regex = ~r/אהבה/
replacement_text = "חיים"
updated_text = Regex.replace(regex, original_text, replacement_text)

IO.puts updated_text
# פלט: חיים זה הכל, חיים זה פה ושם.
```

## עיון מעמיק:
חיפוש והחלפה הם כלים עתיקים יומין בתכנות. הפונקציות של חיפוש והחלפה ב-Elixir מושפעות מהביטויים הרגולריים של Perl, ששינו את המשחק בשנות ה-80. קיימות אלטרנטיבות כמו פעולות חיפוש והחלפה במערכות עיבוד טקסט או בסביבות עריכת קוד, אבל התכונה שב-Elixir מאפשרת יותר גמישות ושליטה מושלמת מתוך הקוד עצמו. הפונקציה `String.replace/3` היא פשוטה לשימוש עבור החלפות ישירות. עבור תבניות מורכבות יותר, `Regex.replace/4` מאפשרת לנו להפעיל ביטויים רגולריים ולבצע החלפות מורכבות יותר.

## ראה גם:
- [Elixir's String module](https://hexdocs.pm/elixir/String.html)
- [Regex module in Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Programming Elixir by Dave Thomas](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
