---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזת היא הדרך שלנו להכניס ערכים משתנים או ביטויים שמחזירים ערך לתוך מחרוזת. הסיבה שלנו לעשות זאת היא לייצר מחרוזות דינמיות שמשתנות בהתאם לנתונים שנשנים.

## כיצד:
להלן שני משלים של שימוש באינטרפולציה של מחרוזת בשפת Ruby:

```Ruby
name = "David"
puts "Hello, #{name}"   # "Hello, David"

age = 30
puts "I am #{age} years old."  # "I am 30 years old."
```
## לעומק:
אינטרפולציה של מחרוזת היא תכונה שנפוצה בשפות תכנות רבות והיא נמשכת מימי שפות כמו C ו-PHP.
באופן אלטרנטיבי, ניתן להשתמש במתודה של האיחוד של מחרוזות, אך ברוב המקרים אינטרפולציה היא הבחירה המועדפת משום שהיא מקנה קריאות משופרת לקוד.
מהצד המימוש, אינטרפולציה של מחרוזת ב-Ruby מתבצעת על ידי גישה לכל מחרוזת כמערך של תווים, ועל ידי החלפת השונה של מרכאות סוגריים {} עם ערך הביטוי שעורף בפנים.

## ראה גם:
- [String interpolation in Ruby](https://en.wikipedia.org/wiki/String_interpolation#Ruby)
- [Ruby documentation: String](https://ruby-doc.org/core-2.7.0/String.html) 