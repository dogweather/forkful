---
title:                "Ruby: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# למה
HTTP פניות עם אימות בסיסי הם דרך חשובה לקשר עם שרתים חיצוניים ולשלוח ולקבל מידע באתרים אינטרנט.

## איך לעשות זאת
בהתחשב בכך שיש לכאלו פניות HTTP שאריתרמיות המתכנת צריך להפעיל אימות כדי לקבל גישה לאתר אינטרנט, הנה דוגמא לכיצד לבצע זאת בקוד רובי:

```ruby
require 'net/http'

uri = URI('http://example.com')

# צור בקשת GET עם אימות בסיסי
req = Net::HTTP::Get.new(uri)
req.basic_auth 'username', 'password'

# שלח את הבקשה וקבל את התגובה בחזרה
res = Net::HTTP.start(uri.host, uri.port) do |http|
  http.request(req)
end

# תמצא את התוכן של התגובה
puts res.body
```

פקודת ה- `require` מפעילה את מודול ה- `net/http` שבתוך פיטריות הרובי ומאפשר לנו ליצור ולשלוח פניות. נולד ייצוג יצירה, אנו אוקדם את הכניסה וinline רחנות קוד איכותי לנתחים פעיל (מי שבכוח רובי 2: מקרה אישי (גישה אאוטו לנתחי הֲתגִיבִים של MCP) כגון LoadBalancer, שם פריצה יחידה תוכל להתייחס אלינו!

כדי לקבל אישורים כבר המתוך שרת האתר החיצוני, אנו משתמשים בפקודת `basic_auth` כדי לציין שם משתמש וסיסמה. לאחר מכן, שולחים את הבקשה לאתר החיצוני באמצעות פקודת `start` ומקבלים תגובה חזרה בתוך משתנה כמו שנראה בדוגמא.

## Profunfo נדיר
אם אתה מעוניין ללכת עמוק יותר בשיטות של HTTP אימות בסיסי, ישנן כמה פקודות וטכניקות יחודיות שניתן להשתמש בהם. הנה כמה דוגמאות:

- שיטת אימות מעורבת עם שם משתמש, סיסמה ומפתח מ