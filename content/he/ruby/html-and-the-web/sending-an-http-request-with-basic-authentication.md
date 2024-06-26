---
date: 2024-01-20 18:02:47.126422-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E7\u05D8\u05E2\
  \ \u05D6\u05D4 \u05D9\u05DB\u05D9\u05DC \u05D3\u05D5\u05D2\u05DE\u05D4 \u05E9\u05DC\
  \ \u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD \u05D0\
  \u05D5\u05D8\u05E0\u05D8\u05D9\u05E7\u05E6\u05D9\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\
  \u05EA \u05D1-Ruby."
lastmod: '2024-03-13T22:44:40.203531-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05D8\u05E2 \u05D6\u05D4 \u05D9\u05DB\u05D9\u05DC \u05D3\u05D5\u05D2\
  \u05DE\u05D4 \u05E9\u05DC \u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA\
  \ HTTP \u05E2\u05DD \u05D0\u05D5\u05D8\u05E0\u05D8\u05D9\u05E7\u05E6\u05D9\u05D4\
  \ \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05D1-Ruby."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

## איך לעשות:
קטע זה יכיל דוגמה של שליחת בקשת HTTP עם אוטנטיקציה בסיסית ב-Ruby:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/secret-page')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'user', 'password'

response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(req)
end

puts response.body
```

אם האימות עבר בהצלחה, התגובה תכיל את גוף הדף הסודי:

```
<secret content here>
```

## צלילה לעומק:
שליחת בקשות עם אוטנטיקציה בסיסית היא פרקטיקה מתחילת ימי האינטרנט. זה סוג של אוטנטיקציה שבו שם המשתמש והסיסמה נשלחים בראש הבקשה (header), מקודדים ב-base64. כיום, יש אלטרנטיבות יותר בטוחות כמו OAuth, אבל basic auth עדיין נפוץ בגלל פשטותו. צריך לזכור להשתמש ב-HTTPS כדי להבטיח את סודיות הנתונים שנשלחים.

## ראו גם:
- [RFC 7617 – The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Net::HTTP Documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html) - מידע על ניהול בקשות HTTP בRuby.
