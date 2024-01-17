---
title:                "הורדת עמוד אינטרנט"
html_title:           "Elixir: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
להוריד עמוד אינטרנט היא פעולה שמתבצעת בעזרת תיכנות ומאפשרת לנו להציג תוכן מקוון במחשב שלנו. תכניתנים מבצעים זאת בכדי לאפשר למשתמשים לגשת לתוכן של אתרי אינטרנט ולייצר יישומים מתקדמים באמצעות תוכן מתוך אתרים.

## איך לעשות?
```Elixir
 defmodule DownloadPage do 
  def main(url) do 
    url
    |> HTTPoison.get()
    |> scrape_page()
  end
end

def scrape_page({:ok, %HTTPoison.Response{status_code: 200, body: body}}) do
  {:ok, body}
end
```
קוד זה משתמש בפונקציות מובנות ב-Elixir כדי להוריד את הדף מהאתר שנמצא בכתובת ה-URL שנתנו לתוכנית. תוצאת הפלט היא התוכן של הדף המורד. 

## נכנסים עומק
הבאסיס היסטורי להורדת עמודים מהאינטרנט היא בעיקר קשור לפיתוח תקשורת ורשתות ב-תכנות. תכניתנים משתמשים בפעולה זו כדי לאפשר גישה לתוכן גלובלי מתוך אתרי אינטרנט ולייצר יישומים מתקדמים כגון בקשות HTTP והנתונים המתקבלים. ישנם כמה פתרונות אחרים להורדת דפי אינטרנט כגון אוססכיפט, פייטון ופרל אך ה-Elixir הוא הפתרון המועדף והממוקד ביותר על ידי מגוון גדול של תכניתנים. 

## ראו גם
למידע נוסף על שפת התכנות Elixir ומדריכים נוספים לפיתוח יישומים אינטרנטיים, ראו:

- המדריך הרשמי של Elixir
https://elixir-lang.org/getting-started/introduction.html

- הספר "Programming Elixir" של Dave Thomas 
https://pragprog.com/titles/elixir16/programming-elixir-1-6/

- הספר "Elixir in Action" של Sasa Juric 
https://www.manning.com/books/elixir-in-action-second-edition