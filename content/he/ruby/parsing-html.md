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

## למה

אנשים משתמשים בניתוח HTML כדי לקרוא ולכתוב מידע מאתרים ודפי אינטרנט. זהו כלי חשוב בתהליך פיתוח אתרי אינטרנט ואפליקציות ומאפשר לקבל וליצור מידע בפורמט מתאים לשימוש.

## איך לעשות זאת

הניתוח של קוד HTML נעשה בעזרת הספרייה Nokogiri הקיימת בשפת רובי. ניתן להתקין את Nokogiri בעזרת פקודת הטרמינל הבאה:

```ruby
gem install nokogiri
```

כדי לפתח קוד מתאים לניתוח HTML, ניתן להשתמש בפעולות כמו נתיבים לאלמנטים, שמות של מחלקות ועוד. לדוגמה, נשתמש בפקודה הבאה כדי לקרוא מידע מתוך דף אינטרנט ולהדפיס את הכותרת הראשית שלו:

```ruby
require 'open-uri'
require 'nokogiri'

doc = Nokogiri::HTML(open("https://www.example.com"))
puts doc.at_css("h1").text
```
הפלט שנקבל יהיה:

```
Welcome to Example Website!
```

בנוסף, ניתן להשתמש בפקודות נוספות כדי לחפש, לקבץ ולערוך מידע אחר מתוך קוד HTML.

## ניתוח עמוק

הספרייה Nokogiri מאפשרת לנו להגיע לשכבות עמוקות יותר בקוד HTML ולעשות מאמצים בדיוק יותר. אם נרצה, נוכל להעריך תוכן שבתוך אלמנט מסוים ולשנות אותו. הספרייה מתמקדת גם במניפולציות של מידע בפורמט הומניזה כדי להציג מידע מובנה יותר למשתמשים מתוך הקוד HTML.

## ראו גם

- [ספריית Nokogiri](https://nokogiri.org/)
- [מדריך לניתוח HTML בשפת רובי](https://www.rubyguides.com/2018/10/parsing-html-nokogiri/)
- [פרויקט Github של Nokogiri](https://github.com/sparklemotion/nokogiri)