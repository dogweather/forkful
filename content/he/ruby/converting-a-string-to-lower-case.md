---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרת מחרוזת לאותיות קטנות היא תהליך שבו אנו משנים את כל האותיות הגדולות במחרוזת לאותיות קטנות. מתכנתים מבצעים זאת כדי לאפשר השוואת מחרוזות לא משנה את הגודל של האותיות.

## כיצד
התרגיל הבא מדגים כיצד להמיר את האותיות הגדולות במחרוזת של "Hello World" לאותיות קטנות:

```Ruby 
string = "Hello World"
puts string.downcase
``` 

הפלט של התרגיל הוא:
``` "hello world"```

## הצולעת
רובי מאפשרת המרה של מחרוזות לאותיות קטנות מאז הגירסה הראשונה שלה. ניתן לבצע את המרה ישירות באמצעות מתודה integrative שנקראת `downcase`. כמו כן, ניתן להריץ משלוח של מחרוזת באמצעות `map`, אך זו התמודה פחות מועדפת, שכן היא מספקת ביצועים פחותים. 

## ראה גם
* [מדריך Ruby על מחרוזות](https://ruby-doc.org/core-2.7.0/String.html)
* [מבוא למחרוזת של Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
* [מטודות מחרוזת ברובי](https://www.rubyguides.com/2018/01/ruby-string-methods/)