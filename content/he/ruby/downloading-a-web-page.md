---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת עמוד אינטרנט היא תהליך שבו הורדת את תוכן עמוד אינטרנט מהשרת אל המחשב המקומי שלך. מתכנתים עשויים לרצות לעשות זאת כדי לנתח את התוכן, לאחסן את המידע כגיבוי, או לאפשר קריאה בלתי מחוברת לאינטרנט.

## כיצד לעשות זאת:
נהיה מנומסים אל השרת ונבקש את התוכן שאנחנו רוצים, באמצעות ביבליות ראשוניות של Ruby . הנה דוגמה:

```Ruby
require 'net/http'

url = URI('http://example.com')
response = Net::HTTP.get(url)
puts response
```

אם אתם רצים להוריד עמוד שנמצא תחת HTTPS, יש לשנות את הקוד קצת:

```Ruby
require 'net/http'

url = URI('https://secure.example.com')
http = Net::HTTP.new(url.host, url.port)
http.use_ssl = true
response = http.get(url)
puts response.body
```

לאחר שהרצת את הקוד, תוכן העמוד יהא בהדפסה.

## הצצה לעומק:
למרות שספריות המוסד של Ruby כוללות כלים להורדת דפי אינטרנט, ישנם ספריות של צד שלישי, כמו `open-uri` ו־ `httparty`, שמקלות על התהליך ומציעות יכולת ניתוח משוגרת.

במשך שנים, חלפו שיטות רבות להורדת האינטרנט. מספר השינויים המרכזיים הם התנהלות עם אתרי HTTPS והתלת מבנים XML/HTML של תוכן העמוד. 

## ראה גם:
1. [הספרייה הרשמית של Ruby ה-Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
2. [HTTParty - ספרייה של צד שלישי לשליחת בקשות HTTP בRuby](https://github.com/jnunemaker/httparty)
3. [Open-URI - ספרייה בנויה לתוך Ruby לפתיחת URIs](https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html)