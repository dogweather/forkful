---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פרסור HTML הוא התהליך שבו אנו מפרשים קוד HTML לנתונים שהוא מייצג. מתכנתים עושים את זה להגיש נתונים מדף אינטרנט בצורה מבנית.

## איך:
הנה דוגמא של קוד Ruby שמשתמש ב-gem שנקרא 'nokogiri' לפרסור HTML.

```Ruby
require 'open-uri'
require 'nokogiri'

url = 'http://example.com'
doc = Nokogiri::HTML(open(url))

doc.css('h1').each do |title|
  puts title.text
end
```
הפלט של דוגמא זו יהיה כל הכותרות מסוג h1 מהאתר http://example.com.

## צלילה עמוקה :
הפרסור של HTML שונה קצת מפרסור שאר שפות התכנות משום ששפת HTML היא שפה שמאוד חשיפה לשגיאות. פרסרים של שפות אחרות לא ירחמו, אך ה- HTML פרסרים נותנים לנו מעין התמרמרות על שגיאות סינטקס. אלטרנטיבות ל- 'nokogiri' כוללות gems כמו 'hpricot' ו- 'oga'. 'Nokogiri', אך ורק, נהיה למועדף בשל flexability ומהירות שלו. הוא מאפשר למתכנת לבחור את המנוע שעליו הפרסר ירוץ - yaoba או REXML.

## ראה גם:
1. [Nokogiri](https://nokogiri.org/)
2. [Hpricot](https://github.com/hpricot/hpricot)
3. [Oga](https://github.com/YorickPeterse/oga)

אין לשכוח מהבנות HTML משום שהן משחקות תפקיד חיוני ביצירת קוד ראוי. יותר מאשר כלי, הן הבחירה של מאמץ.