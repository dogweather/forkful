---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Ruby: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה:
מחיקת תווים שתואמים דפס מסוים יכולה להיות כלי חזק לעיבוד טקסט ותיקונו. זה יכול לאפשר למשתמשים למצוא ולהסיר מידע חסום או לא רצוי מתוך שורות טקסט ארוכות.

## כיצד לעשות זאת:
מסירת תווים שתואמים דפס מסוים ב-Ruby ניתן לבצע באמצעות פונקציית `gsub`, שמקבלת כפרמטרים את הדפוס לחיפוש ואת התווים החדשים שיחליפו אותו. לדוגמה:

```Ruby
text = "Hello, world!"
new_text = text.gsub("o", "")
puts new_text #=> "Hell, wrld!"
```

במקרה של מחיקת תווים כפולים, ניתן להשתמש בדגש (`\1`) כדי להצביע על תיבות התפוסה. כך ניתן להשתמש בתבנית כדי להסיר את הכפילויות מהתווים:

```Ruby
text = "Wow!!!"
new_text = text.gsub(/(.)\1+/, "\1")
puts new_text #=> "Wow!"
```

## חקירה מעמיקה:
פונקציית `gsub` מובנת בשפת Ruby ומאפשרת למשתמש להתאים ולהחליף תווים בטקסט בצורה יעילה. הפונקציה תמיד מחזירה מחרוזת חדשה ואינה משנה את המחרוזת המקורית. בנוסף, ישנם מרכיבים נוספים שניתן להוסיף לפונקציה כדי להשפיע על התחלת החיפוש, כמו טווח תווים או תבניות רגולריות.

## ראו גם:
- [מדריך לפונקציית `gsub` ב-Ruby](https://www.rubyguides.com/2018/05/ruby-gsub-method/)
- [תיעוד רשמי לפונקציית `gsub` ב-Ruby](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub)