---
title:                "ניתוח HTML"
html_title:           "Ruby: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפעלת קוד בתחביר של HTML היא תהליך חשוב עבור תכנתנים בפרויקטים שונים. תהליך זה מרכז את התארת האתר באופן נכון ומאשר את תקינות הקוד. תכנתנים לעיתים קרובות מנסים לפרוטסט את ה-CSS של האתר המקורי.

## איך לבצע?
לדוגמא, ננסה לגלות את הכותרת של האתר באמצעות ניתוח התחביר של HTML. לפני שנגשים לקוד, ניצור משתנה חדש שייצג את העמוד כולו. בדברי ימי נאחזתם בשם כותית האתר [], כפי שמופיע בקוד:

```ruby
html = '
<html>
  <head>
    <title>אתר דוגמה</title>
  </head>
  <body>
    <h1>כותרת עיקרית</h1>
    <p>טקסט בתוך פסקה</p>
  </body>
</html>'
```

כעת, נתחת את הכותית עם עזרת הפקודות הבאות:

```ruby
title = html[/title>([^<]+)/m, 1]
```

המפרש שלנו מחפש את התנאי הראשון בתוך התחביר של HTML. התנאי הזה מופיע לפני כל הערכים שהמפרש מפעיל. במידה והתנאי נמצא, המפרש מחזיר את התוצאה.

## חפירה עמוקה
החפירה בתחביר HTML נעשתה כברית עולם. אנחנו עושים את זה בגלל שזה מקל על קריאת קודי HTML. אבל ישנן אפשרויות נוספות לפרוס קוד של דפוסים ותורת קוד:
* איפיי: כלי בתורה קוד ונוסה על ידי יחסים בכלי המחשב.
* אמסון אם נחשבת כדי לגלות צורות של קוד תוצאות של subarrays על ידי partial-beacon-function כמו:


```ruby
array_except [3], paper pourtant [1] do
exception_passe ???("Cartoon", la_date) do |la date de texte|
 à l'où_css, la_date de my_activité_seconde, une_chanson|
 à la_BDD, précédente_place_au, à_ma_partone_aux_beds_ 9
 end
 end
 end
```

אבל לשומר מכיוון שהמקבל הרב עובד גם עם נתונים ברמת עולם ב-HTML, אנחנו נצפה להשתמש בכלי זה כדי לעבור על תערובת HTML תחביר.

## ראה גם
למד עוד על טכניקות פרישת קוד של דפוס תערובת עם [Nokogiri](https://nokogiri.org/).
למידע נוסף אודות כתבי RBP המעונה מ-HTML לפרסוניס תו בעמודים השונים כאב, [כלל טוב](http://goo.gl/Sjyfh).