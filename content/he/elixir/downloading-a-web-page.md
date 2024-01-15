---
title:                "הורדת דף אינטרנט"
html_title:           "Elixir: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

בעולם המודרני, כמעט כל תוכן נמצא באינטרנט. אם אתם מפתחים אפליקציה או מסתכלים לחקור אתר אינטרנט מסוים, אתם כנראה תצטרכו להוריד דף אינטרנט מסוים. במאמר זה אנחנו נלמד איך להוריד דף אינטרנט באמצעות פרוגרמה בשפת Elixir.

## כיצד לעשות זאת

כדי להוריד דף אינטרנט בשפת Elixir, יש להשתמש בפונקציה HTTPoison.get. באמצעות פונקציה זו, ניתן לשלוח בקשת HTTP ולקבל תגובה מהאתר הנמצא בכתובת המתאימה.

```elixir
response = HTTPoison.get("https://www.example.com")

IO.inspect response
```

פלט: 

```elixir
%HTTPoison.Response{
  body: "...", # קוד HTML של הדף המבוקש
  headers: [...], # כותרות הכותרת של הדף המבוקש
  status_code: 200 # קוד סטטוס 200 מציין שהבקשה נקלטה בהצלחה
}
```

## התעמולה העמוקה

פונקצית HTTPoison.get היא חלק מספריית HTTPoison בשפת Elixir. ספריית זו מאפשרת למפתחים לתקשר עם שרתי HTTP ולבצע פעולות שונות על דפי אינטרנט. ישנם פונקציות נוספות בספרייה זו שמאפשרות לשלוח בקשות מתקדמות, להחזיר קבצים מהאינטרנט ועוד.

## ראו גם

כדי ללמוד עוד על ספריית HTTPoison ועל דרך שימוש בה, נדרשת יכולת בסיסית בשפת Elixir. אלו הם כמה מקישורי התאמץ המומלצים על הנושא:

- הדף הרשמי של ספריית HTTPoison: https://hexdocs.pm/httpoison/HTTPoison.html
- האתר הרשמי של שפת Elixir: https://elixir-lang.org/
- אתר העזרה המקצועי של Elixir: https://elixir-lang.org/resources.html