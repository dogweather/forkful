---
date: 2024-01-20 17:58:08.631458-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05D4\
  \ \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8 \u05D4\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\
  \u05EA \u05E9\u05D1\u05D4\u05DF \u05D0\u05E0\u05D5 \u05DE\u05D0\u05EA\u05E8\u05D9\
  \u05DD \u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA \u05E9\
  \u05DC \u05D8\u05E7\u05E1\u05D8 \u05D5\u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD\
  \ \u05D0\u05D5\u05EA\u05D4 \u05D1\u05D0\u05D7\u05E8\u05EA. \u05EA\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05E2\u05D3\u05DB\u05DF \u05D1\u05D0\u05D5\u05E4\u05DF \u05DE\
  \u05D4\u05D9\u05E8 \u05DE\u05D9\u05D3\u05E2, \u05DC\u05EA\u05E7\u05DF \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA, \u05D0\u05D5\u2026"
lastmod: '2024-03-11T00:14:12.170570-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05D4 \u05E9\
  \u05DC \u05D8\u05E7\u05E1\u05D8 \u05D4\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\
  \ \u05E9\u05D1\u05D4\u05DF \u05D0\u05E0\u05D5 \u05DE\u05D0\u05EA\u05E8\u05D9\u05DD\
  \ \u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA \u05E9\u05DC\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D5\u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD \u05D0\
  \u05D5\u05EA\u05D4 \u05D1\u05D0\u05D7\u05E8\u05EA. \u05EA\u05DB\u05E0\u05D9\u05EA\
  \u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\
  \u05D9 \u05DC\u05E2\u05D3\u05DB\u05DF \u05D1\u05D0\u05D5\u05E4\u05DF \u05DE\u05D4\
  \u05D9\u05E8 \u05DE\u05D9\u05D3\u05E2, \u05DC\u05EA\u05E7\u05DF \u05E9\u05D2\u05D9\
  \u05D0\u05D5\u05EA, \u05D0\u05D5\u2026"
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
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
