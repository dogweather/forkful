---
title:                "הפיכת מחרוזת לאותיות גדולות"
html_title:           "Elixir: הפיכת מחרוזת לאותיות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות גדולות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

שינוי האות הראשונה של מחרוזת לאות ראשונה גדולה נקרא "הגבהת מחרוזת". זה נעשה כדי להדגיש/להבליט מידע כלשהו, או למדוד את התאימות לפורמט מסוים.

## איך נעשית זאת:

אפשר להגביה מחרוזת באליקסיר עם הפונקציה `String.capitalize/1`. יש לך להראות לדוגמה:

```Elixir
IO.puts String.capitalize("hello world")
```

זה ייצא:

```Elixir
Hello world
```

## צלילה עמוקה:

אף אחד לא יכול להיות בטוח, אך נראה שהחלה התקנת מונח "הגבהת מחרוזת" באנגלית מסביב לשנות ה-60 כשהמחשבים נהיו לפופולאריים. ישנן גם אלטרנטיבות לפונקציה שלנו, כמו  `titlecase/1` שאינה מהפכת את האות בפותחת פסקא. לחלופין, אם תצטרך להעלים את כל האותיות, שימוש ב `String.upcase/1` יהיה נכון. בהנחה שאנו מבצעים את הגבהה, מהלך העיבוד משנה את האות הראשונה של המחרוזת לאות גדולה תוך שמירה על האותיות האחרות כאותיות קטנות.

## ראו גם:

דף המסמך הרשמי של ספרייה רדיקס `String` באליקסיר הוא מקור מצוין למידע נוסף על `String.capitalize/1` ואחרות: 
[https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)