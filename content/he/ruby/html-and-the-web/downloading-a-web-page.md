---
title:                "הורדת דף אינטרנט"
aliases:
- /he/ruby/downloading-a-web-page.md
date:                  2024-01-20T17:44:41.639212-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/downloading-a-web-page.md"
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
