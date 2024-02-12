---
title:                "שימוש בדיבאגר"
aliases:
- /he/ruby/using-a-debugger.md
date:                  2024-01-26T04:10:30.635726-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בדיבאגר"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/using-a-debugger.md"
---

{{< edit_this_page >}}

## מה ולמה?

שימוש במנפה שגיאות (debugger) ברובי מעניק למתכנתים כוח-על להשהות את הקוד שלהם, לבחון משתנים ולעבור על הקוד שלהם שורה אחר שורה. אנשים עושים זאת כדי לדכא באגים, להבין את זרימת הקוד ולראות בדיוק מה השפות הכתובות (הקוד) שלהם עושות כשהקסם קורה - או לא.

## איך לעשות:

רובי מגיעה עם מנפה שגיאות מובנה בשם `byebug`. ראשית, כלול את `byebug` בקובץ Gemfile שלך והרץ `bundle install`. לאחר מכן, שחוק את `byebug` בדיוק במקום בו אתה רוצה שהתוכנית שלך תעשה הפסקה.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

הרצת סקריפט זה תעצור את הביצוע בנקודת `byebug`, ואתה תזרק לסשן אינטראקטיבי שבו אתה יכול להקליד פקודות כמו:

```
step
next
continue
var local
```

תדפיס פלט דוגמא שנראה כך:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## צלילה עמוקה:

הרבה לפני `byebug`, תכנתני רובי (Rubyists) השתמשו ב`debugger` וב`pry`. האחרון, `pry`, הוא יותר מפורש שגיאות; הוא REPL חזק שניתן גם להשתמש בו לאיתור באגים עם נקודת השביתה `binding.pry`.

חלופות ל`byebug` של רובי כוללות את `pry-byebug`, שמשלב בין `pry` לתכונות של `byebug`, ואת `ruby-debug`, שהוא גימ יותר ישן שאינו מתוחזק באופן פעיל.

כאשר אתה קורא ל`byebug`, הפורש שגיאות משהה את ביצוע הקוד שלך ונותן לך הצצה לתוך זמן הריצה. אתה יכול לראות ולשנות משתנים, לקפוץ לנקודות שונות בקוד, ואף להריץ קוד רובי שורה אחר שורה. זה כמו להיות בעל יכולות זמן-נסיעה לקוד שלך ברובי.

## ראה גם:

- מאגר ה-GitHub של Byebug: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- תיעוד Pry: [https://github.com/pry/pry](https://github.com/pry/pry)
- מדריך לאיתור שגיאות באפליקציות Rails: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
