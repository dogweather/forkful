---
title:                "שליחת בקשת http"
html_title:           "Ruby: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

שהבנתי יש 3 סיבות עיקריות מדוע לשלוח בקשת HTTP:

1. האתר או היישום שאנו משתמשים בו זקוק לקבלת מידע חדש או עידכון על מצב מסוים.
2. אנו רוצים לשלוח מידע לשרת כדי לבצע פעולה מסוימת (לדוגמה, להצביע בסקר או לפרסם תגובה).
3. אנו רוצים ליצור יישומון או כלי שיכול לשלוח בקשות כדי לקבל או לעדכן מידע בצורה אוטומטית.

## איך לשלוח בקשת HTTP

כדי לשלוח בקשת HTTP בשפת רובי, ניתן להשתמש בספריית 'net/http'. למשל:

```Ruby
require 'net/http'

url = URI('https://www.example.com')
http = Net::HTTP.new(url.host, url.port)
http.use_ssl = true # אם אתר משתמש בחיבור מאובטח
response = http.get(url) # שליחת בקשת GET לאתר

puts response.code # קוד התגובה (200 במקרה של הצלחה)
puts response.body # גוף התגובה (המידע שמקבלים מהאתר)

```

כמו כן, ניתן להשתמש גם בספריית 'httparty', המאפשרת שליחת בקשות פשוטה ומובנית מאוד. לדוגמה:

```Ruby
require 'httparty'

response = HTTParty.get('https://www.example.com')

puts response.code
puts response.body
```

כדי לשלוח בקשות POST או PUT, ניתן להשתמש בפעולות כמו `http.post` או `http.put` ולציין בתוכן של הבקשה את המידע שרוצים לשלוח.

## צילום מקורי

HTTPS (Hypertext Transfer Protocol Secure) הוא פרוטוקול המאפשר שירותים שונים על האינטרנט, כמו העברת קבצים, תצוגת אתרים, או שליחת אימיילים. כאשר אנו שולחים בקשות באמצעות הפרוטוקול הזה, אנו יוצרים קשר דינמי עם השרת ומתחילים לבצע תקשורת ישירה אליו.

## ראו גם

כאן תוכלו למצוא מידע נוסף על שליחת בקשות HTTP ועל הפרוט