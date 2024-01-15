---
title:                "הורדת דף אינטרנט"
html_title:           "Ruby: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

מחשבים יכולים לראות רק מידע שנשלח להם, אז כאשר אנו גולשים באינטרנט אנו צריכים להוריד את הדף הרלוונטי כדי לגלות את התוכן שבו.

## איך לעשות זאת

כדי להוריד דף אינטרנט בשפת רובי, נוכל להשתמש בתוכנית קטנה עם מספר שורות קוד.

```Ruby
require 'net/http'

url = URI("https://www.example.com") #הכנסת כתובת האתר שברצונך להוריד
response = Net::HTTP.get(url) #השמת מידע במשתנה response
puts response #הדפסת מידע
```

כאן, אנו משתמשים בספריה של נט ה-HTTP בשביל ליצור חיבור לכתובת האתר. אחרי זה, אנו משיגים את התוכן של הדף ומדפיסים אותו. והנה, אנו יכולים לראות את התוכן של הדף המכיל את כל התכנים, תמונות וקישורים.

## חפירה עמוקה

ניתן לעשות את הפעולה של הורדת דף אינטרנט בצורה יעילה יותר עם גמישות דרך השתמשות בכלי של "Nokogiri". כלי זה מאפשר לנו לעבד ולחשוב על תגי HTML במילות פשוטות כדי לקבל רשימת תוכן ולהנות ממודל התיעוד. ניתן למצוא דוגמאות מפורטות לשימוש בכלי זה [כאן] (https://nokogiri.org/).

## ראה גם

- [Short Ruby Guide - Introduction to Web Scraping with Ruby] (https://www.rubyguides.com/2018/07/ruby-web-scraping/)
- [Nokogiri Documentation] (https://nokogiri.org/)