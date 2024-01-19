---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא התהליך שבו מחשב נותן בקשה לשרת על מנת לגשת למידע. מתכנתים שולחים בקשת HTTP כאשר הם רוצים שאפליקציה שלהם תתקשר עם שרת.

## איך?
נפתח Elxir וניצור HTTP request באמצעות הספרייה HTTPoison.

```Elixir
HTTPoison.get("http://mywebsite.com")
```

התגובה תחזור כמבנה עם שני אלמנטים: הסטטוס של הבקשה והתגובה עצמה.

```Elixir
{:ok, %HTTPoison.Response{body: body, status_code: status_code}}
```

## צלילה עמוקה
שליחת בקשת HTTP התפתחה בשנות ה-90 כחלק מהגדרת האינטרנט. ישנן חלופות ל-HTTPoison ב-Elixr, כולל :hackney, :httpc. בניגוד לרוב הספריות, HTTPoison מצטיין במשקת ה-API שלה שהיא פשוטה ונגישה.

## ראה גם
עמוד התיעוד HTTPoison: "https://hexdocs.pm/httpoison/readme.html"
מדריך עבודה עם אוכלוסיות(CONTAINERS) Elixir : "https://www.howopensource.com/2019/04/elixir-containers-tuples-lists-maps-keywords/"
פוסט בנושא: "https://www.monterail.com/blog/2014/elixir-http-request-using-httpoison"