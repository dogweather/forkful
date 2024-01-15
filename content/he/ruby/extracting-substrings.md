---
title:                "חילוץ תת-מחרוזות"
html_title:           "Ruby: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה
קיצור סדרת תווים הוא כלי מאוד שימושי בתכנות ברובע של אינדקסים ותת־מחרוזות. זה מאפשר לסנן ולהתאים מחרוזות לפי הצורך וליצור קוד יעיל וקריא.

## איך לעשות זאת
לקצר תווים ב־Ruby, ניתן להשתמש בשלושה אופציות שונות: מתודת `slice`, פעולות הגלישה של סדרת התווים `[]` ובשימוש במתודה `gsub`. נהדור קצת קוד שימושי כדי להמחיש כיצד יכול להיראות הקצרת של תווים בתוך מחרוזת:

```Ruby
my_string = "Hello world!"
puts my_string.slice(2, 5) # Outputs "llo w"
puts my_string[0..4] # Outputs "Hello"
puts my_string.gsub("o", "") # Outputs "Hell wrld!"
```

## גילוי מעמק
באמצעות תכונת הארגומנטים השליליים של `slice`, אנו יכולים לקצר תווים על סמך מיקוםם האחורי בתוך המחרוזת. נוסף לכך, ניתן להשתמש במתודה `slice!` כדי לקצר את התווים במקום בצורה ישירה בתוך המשתנה. כמו כן, ניתן לעשות קצירה לפי אופציות מסוימות כמו תווים מסוימים או אופציות של נקודת התחלה וסוף. נהדור קוד נוסף כדי לראות כיצד ניתן לתת את עצמנו יותר בכלים כדי לקצר תווים:

```Ruby
my_string = "Welcome to the Ruby programming language"
puts my_string.slice(-10..-1) # Outputs "language"
puts my_string.slice!(0, 7) # my_string now equals "to the Ruby programming language"
puts my_string[9..-1] # Outputs "he Ruby programming language"
```

## ראו גם
לכדי להיות מתכנת יעיל ב־Ruby, כמו תמיד, למדנו כיצד למצוא תווים בתוך מחרוזת. אם אתם מעוניינים ללמוד עוד על פעולות מחרוזות ב־Ruby, תוכלו לבדוק את המאמרים הבאים:

- [Working with Strings in Ruby](https://hackr.io/blog/working-with-strings-in-ruby)