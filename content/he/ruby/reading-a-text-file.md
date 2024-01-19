---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט היא התהליך שבו מנגנון התוכנה שלנו משיג את המידע המוקלד מתוך קובץ מסוים. אנו בתכנות מבצעים זאת בעיקר כדי למנוע הדפסה של המידע באופן ידני בתוך הקוד.

## איך לעשות:

נציג את הקריאת קובץ טקסט באמצעות שפת התכנות Ruby:

```ruby
File.open("testfile.txt") do |file|
  while line = file.gets
    puts line
  end
end
```

התוצאות שנקבל:

```ruby
This is line 1
This is line 2
This is line 3
```

## צלילה עמוקה:

(1) מהלך הקריאה של קובץ טקסט בשפת התכנות Ruby היה אחד מהמאפיינים הראשונים של השפה, המופיע בראשון כבר מעידן של "Ruby 0.95" ב-1995. 

(2) ישנם שיטות אחרות לקריאת קובץ טקסט כולל "IO.read" או "IO.foreach". הבחירה מתבצעת על פי הצרכים של התכנית.

(3) הדרך שבה Ruby מנהלת את הפרטים הממומשים של קריאת קובץ תלויה במערכת ההפעלה. לדוגמא, בגרסת Unix של Ruby, הדרך שבה הקריאה מתבצעת היא בעזרת פקודות של מערכת ההפעלה.

## ראה גם:

[עזרה בשפת התכנות Ruby](https://ruby-doc.org/core-2.7.1/File.html)

[הסברים על קריאת קובץ טקסט](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)

[דוגמאות לקריאת קובץ](https://www.geekhideout.com/ruby-read-file.shtml)