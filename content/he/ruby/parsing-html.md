---
title:                "Ruby: פירוק html"
simple_title:         "פירוק html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## למה:

אם אתם בעלי אתר או מפתחי תוכנה, ייתכן שתגידו שפרסום תוכן חדש באתר שלכם הופך להיות משולש בראש על גבי מערכת ניהול התוכן שלכם. אחת הדרכים לפתרון בעייה זו היא על ידי פירוק קוד HTML וטיפול בו באופן דינמי בעזרת שפות תכנות כמו רובי.

## איך לעשות זאת:

נוצרה ספרייה בשפת רובי בשם "Nokogiri" המאפשרת פירוק, חיפוי וניהול של קוד HTML. הנה דוגמא לשימוש בספרייה זו כדי להשיג את תוכן הכותרת הראשית של אתר:

```Ruby
require 'nokogiri'
require 'open-uri'

page = Nokogiri::HTML( open( 'https://www.example.com' ) )
title = page.css('h1').text
puts "הכותרת הראשית של האתר היא: #{title}"
```

פלט:
``` 
הכותרת הראשית של האתר היא: דוגמה של אתר
```

## עומק:

פירוק HTML יכול להיות מאתגר מבחינה טכנית, מבחינת איך פירוק מורכב מבחינת קוד ומבחינת היעילות של האלגוריתמים. יתרה מכך, קיימות מספר טכניקות וכלים מתקדמים לניתוח HTML אשר עשויים להיות חיוניים בעבודתכם כמפתחי רובי.

## ראו גם:

- [תיעוד רשמי של Nokogiri עבור רובי](https://nokogiri.org/)
- [פרויקט פתוח נוסף לניתוח HTML בשפת רובי - Mechanize](https://github.com/sparklemotion/mechanize)
- [מדריך לניתוח וטיפול בקוד HTML בעזרת ספריית Nokogiri](https://www.rubyguides.com/2019/04/parsing-html-nokogiri/)