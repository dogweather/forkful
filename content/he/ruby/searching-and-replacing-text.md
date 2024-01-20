---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

#**מה ולמה?**
חיפוש והחלפה של טקסט הן שני מהשיטות העיקריות שמפתחי תוכנה משתמשים בהם לשנות ולשפר את קוד המקור שלהם. זה מאפשר לנו לשנות מהירים דינמיקות קוד מסוימות באמצעות החלפת קישורים, טקסטים, שמות משתנים ועוד.

#**איך לעשות:**
ניתן לבצע חיפוש והחלפה באמצעות המתודה `gsub`.

```ruby
str = "Hello, World!"
new_str = str.gsub("World", "Ruby")
puts new_str
```

הפלט:

```
Hello, Ruby!
```

כאן, `gsub` מחפש את כל המופעים של המחרוזת "World" ומחליף את זה ב"Ruby".

#**צלילה עמוקה:**
סביבה של רובי מכילה מספר שיטות לחיפוש ולהחלפה של טקסט. `gsub`, שהוזכר להלן, הוא רק אחד מסביבת לכינון משתנים ומחרוזות. `gsub` מקורי בעצם מקורי למדעי המחשב, נפוץ בשלל שפות תכנות אחרות.

ייתכן וישנם חלופות מתקדמות יותר, כמו Regular Expressions (`regex`), שמספקות סביבה גמישה יותר לקריאה והחלפה של הטקסט, אך הן מצריכות למד נוסף והבנה של סינטקס מסוימת.

#**ראו גם:**
* [Ruby Documentation on gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
* [Ruby Regular Expressions Tutorial](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)
* [A Good Book About Ruby](https://pragprog.com/titles/ruby3/programming-ruby-1-9/)