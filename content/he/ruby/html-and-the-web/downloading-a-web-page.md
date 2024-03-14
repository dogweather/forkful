---
date: 2024-01-20 17:44:41.639212-07:00
description: "\u05DE\u05D5\u05E8\u05D9\u05D3\u05D9\u05DD \u05D3\u05E3 \u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05E0\u05D8 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D0\
  \u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC\u05D5 \u05D1\u05EA\u05D5\u05E8\
  \ \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8. \u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05E2\u05D9\u05D1\
  \u05D5\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D1\u05D3\u05D9\u05E7\u05EA\
  \ \u05E0\u05D2\u05D9\u05E9\u05D5\u05EA \u05D0\u05D5 \u05DC\u05DE\u05D8\u05E8\u05D5\
  \u05EA \u05E0\u05D9\u05D8\u05D5\u05E8."
lastmod: '2024-03-13T22:44:40.202003-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D5\u05E8\u05D9\u05D3\u05D9\u05DD \u05D3\u05E3 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D0\u05EA\
  \ \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC\u05D5 \u05D1\u05EA\u05D5\u05E8 \u05E7\
  \u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05E2\u05D9\u05D1\u05D5\
  \u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D1\u05D3\u05D9\u05E7\u05EA \u05E0\
  \u05D2\u05D9\u05E9\u05D5\u05EA \u05D0\u05D5 \u05DC\u05DE\u05D8\u05E8\u05D5\u05EA\
  \ \u05E0\u05D9\u05D8\u05D5\u05E8."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
מורידים דף אינטרנט כדי לקבל את התוכן שלו בתור קובץ טקסט. תכנתים עושים זאת לעיבוד נתונים, בדיקת נגישות או למטרות ניטור.

## How to: (איך לעשות:)
```Ruby
require 'net/http'
require 'uri'

def download_webpage(url)
  uri = URI(url)
  response = Net::HTTP.get(uri)
  File.write('page.html', response)
  puts "Webpage downloaded!"
end

download_webpage('http://www.example.com')

# Output:
# Webpage downloaded!
```

## Deep Dive (עומק השיחה)
בעבר, רובי השתמשה בספריה open-uri להורדת דפי אינטרנט, אבל Net::HTTP היא אופציה יותר נקית לאורך זמן. ישנן אלטרנטיבות כמו RestClient או HTTParty שמספקות ממשקי שימוש קלים יותר אך דורשות התקנת גמס (gem) חיצוני. בבחירת כלי, חשוב לשקול את ענייני האבטחה, נוחות השימוש, ותמיכה בפרויקטים. Net::HTTP מובנית ברובי ולכן לא מחייבת תלות נוספת.

## See Also (ראה גם)
- [HTTParty Gem](https://github.com/jnunemaker/httparty)
- [RestClient Gem](https://github.com/rest-client/rest-client)
