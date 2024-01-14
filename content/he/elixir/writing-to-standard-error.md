---
title:    "Elixir: כתיבה לסטנדרט שגיאה"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Hebrew Translation:

## מדוע

למה לחשוב על כתיבה לשגיאת סטנדרט?

## כיצד לעשות זאת

בתור מתכנתים, כתיבה לשגיאת סטנדרט היא כלי חשוב במהלך הפתרון של בעיות בתוכנית. להלן דוגמא קצרה כיצד לכתוב לשגיאת סטנדרט ב-Elixir:

```Elixir
IO.puts "הודעה לשגיאת סטנדרט"
```

פלט המסך יהיה:

```
הודעה לשגיאת סטנדרט
```

שימו לב שהמלל מודפס תוך שימוש ב-fputs במקום ב-print כדי להבטיח כתיבה לסטנדרט שלמה ולא על חצי השורה הנוכחי.

## חפירה עמוקה

כאשר שירותי האתר שלך מתבשמים למשתמשים, אתה רוצה להבטיח שכל מידע השגיאה נרשם ונמצא בנגישות לשירותי הסיונר העומדים בפניו. כתיבה לסטנדרט שלמה עוזרת גם למצביע בסוף תהליך האימות להבין מה קרה במהלך הריצה.

## ראה גם

- [Elixir תיעוד רישמי: IO] (https://hexdocs.pm/elixir/IO.html)
- [סילבוס: סילבוס הראשי של Elixir] (https://elixir-lang.org/getting-started/introduction.html)
- [אתר אינטרנט: הדבר הבא בפלטפורמת Elixir] (https://www.freecodecamp.org/news/the-next-big-thing-in-the-elixir-platform/)