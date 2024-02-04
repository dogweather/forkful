---
title:                "פיענוח HTML"
date:                  2024-02-03T19:13:20.345293-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
פירוק HTML מתייחס לפירוק חתיכת קוד HTML כדי להבין את מבנהו ותוכנו. תכנתים עושים זאת בכדי לחלץ נתונים, לשנות תוכן, או להעביר מידע בין פורמטים ומערכות.

## איך לעשות זאת:
כדי לפרק HTML בRuby, התקנו את הג'ם 'Nokogiri' עם `gem install nokogiri`. Nokogiri הוא כמו סכין שוויצרי לעבודה עם HTML וXML בRuby. הנה דוגמא מהירה:

```ruby
require 'nokogiri'
require 'open-uri'

# טעינת תוכן HTML מאתר
html_content = URI.open('http://example.com').read

# פירוק הHTML
doc = Nokogiri::HTML(html_content)

# חילוץ הכותרת
title = doc.xpath('//title').text
puts "כותרת הדף היא: #{title}"
```

זה יוציא משהו כמו: `כותרת הדף היא: Example Domain`.

## צלילה עמוקה
בימים הראשונים של Ruby, האפשרויות לפירוק HTML היו מוגבלות. REXML היה מובנה אך איטי. אז הופיע Hpricot, אך הוא דעך. Nokogiri הושק ב-2008, משלב את נוחות השימוש של Hpricot עם המהירות והעוצמה של libxml, ערכת כלים מוכחת ל-XM״L.

בעולם הפירוק, תמיד יש חלופות. חלק מעדיפים את ספריית 'rexml' המובנית או 'oga', פרסר נוסף לXML/HTML עבור Ruby. אך Nokogiri נשאר האהוב בשל חוסנו ומהירותו, שלא לדבר על מערך התכונות העצום שלו.

מאחורי הקלעים, Nokogiri הופך את הHTML למודל אובייקט של מסמך (DOM)—מבנה עץ. זה מקל על הניווט ושינוי האלמנטים. באמצעות XPath ובוררי CSS, ניתן לזהות כל חלק של מידע שנדרש.

## ראה גם
- הג'ם Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- תיעוד rexml של Ruby: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- הפרסר החלופי 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- למידה על XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
