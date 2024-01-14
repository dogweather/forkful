---
title:                "Elixir: פיענוח html"
simple_title:         "פיענוח html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## למה

הקוראים היקרים, אנחנו כולם מכירים את המצב - אתם רוצים לקרוא את התוכן של אתר אינטרנט, אבל כדי להציג אותו באופן נוח לקריאה עליכם לעבור דרך מעטפת ה-HTML המסורבלת. זהו המקום בו הפעילות של פירסום HTML ב-Elixir מציעה פתרון מעולה. עם יכולת לנתח תגי HTML ולהפעיל עליהן פעולות, תוכלו להציג נתונים מהאינטרנט בצורה קלה ונוחה לקריאה. לא רק זה, גם ניתן לבצע שינויים ולייצר אתרים ניידים יותר בקלות עם פירסום HTML ב-Elixir.

## כיצד לעשות זאת

לפניכם דוגמא קטנה של איך להשתמש בכלי פירסום HTML ב-Elixir. בקוד הבא תוכלו לראות כיצד אנו ניתחים את תמונות בעמוד HTML:

```Elixir
html_page = "<html><body><img src="my_image.jpg"></body></html>"

parsed_page = Floki.parse(html_page)

images = Floki.find(parsed_page, "img")

Enum.each images, fn image ->
    IO.puts Floki.attribute(image, "src")
end
```

כאן, אנחנו משתמשים בכלי Floki כדי לנתח את תמונות המצויות בכתובת ה-URL של התמונה. מכאן, אנו מציגים את התמונות בעמוד ומדפיסים את הכתובת המלאה שלהן.

## חפירה עמוקה

כעת, נשטף עמקים עמוקים יותר מתוך שליטה עבור אנחנו מבינים כיצד לקרוא את התכנים של עמוד HTML באמצעות Floki. כיצד זה עובד? כאשר אתם משתמשים בכלי פירסום HTML ב-Elixir, הוא מנתח את קוד ה-HTML ויוצר עץ DOM (Document Object Model) של התמונה. מתוך העץ הזה, אתם יכולים להשתמש בפונקציות כגון "find" כדי למצוא אילו תכנים אתם מעוניינים להציג.

דרגות החופ