---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה & למה?

שליחת בקשת HTTP היא דרך להעביר מידע מהמחשב שלך לשרת. מתכנתים שולחים בקשות HTTP לגשת למידע חיצוני, כמו API, או לשנות מידע בבסיסי נתונים דרך שרתי אינטרנט.

## איך ל:

קוד ב Ruby לשליחת בקשת HTTP:

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://example.com/search")

http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri.request_uri)

response = http.request(request)

puts response.body
```

אם תריצו את הקוד תוך הוספת URL של אתר אמתי, התגובה תהיה HTML של הדף.

## הסוף העמוק

### הקשר ההיסטורי
למדתי שפה שמקודמת ל Ruby להתמודד עם רשת האינטרנט, שפת C, השתמשה בספריות רשת מורכבות. כאשר מתכנתים בנו Ruby, הם רצו ליצור דרך קלה יותר לעבוד עם בקשות HTTP.

### חלופות
ישנן גם ספריות חיצוניות עזר שמספקות דרכים נוספות לשליחת בקשות HTTP, כמו `Rest-Client` ו` HTTParty`. אלה מציעות אפשרויות נוספות אך הן אינן חייבות.

### פרטי ביצוע
בקשת HTTP מתנהלת כאשר המחשב שלך שולח מידע לשרת ובהמשך מקבל תגובה ממנו. הפרוטוקול HTTP מגדיר את הצורה שבה מתבצעת התקשורת הזו.

## ראו גם

"http://www.ruby-lang.org/": האתר הרשמי ל Ruby.
"http://ruby-doc.org/stdlib-2.5.1/libdoc/net/http/rdoc/Net/HTTP.html": מסמך המתיחס ל Net::HTTP ב Ruby.
"https://github.com/rest-client/rest-client": Rest-Client ב GitHub.
"https://github.com/jnunemaker/httparty": HTTParty ב GitHub.