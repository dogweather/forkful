---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
aliases:
- /he/ruby/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:47.126422-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אוטנטיקציה בסיסית מאפשרת לנו לאמת משתמשים באינטרנט באופן פשוט ומאובטח. מתכנתים משתמשים בזה כדי להבטיח גישה אישית למידע רגיש או מוגבל.

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
