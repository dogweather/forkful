---
date: 2024-01-26 04:10:30.635726-07:00
description: "\u05E8\u05D5\u05D1\u05D9 \u05DE\u05D2\u05D9\u05E2\u05D4 \u05E2\u05DD\
  \ \u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05DE\u05D5\u05D1\
  \u05E0\u05D4 \u05D1\u05E9\u05DD `byebug`. \u05E8\u05D0\u05E9\u05D9\u05EA, \u05DB\
  \u05DC\u05D5\u05DC \u05D0\u05EA `byebug` \u05D1\u05E7\u05D5\u05D1\u05E5 Gemfile\
  \ \u05E9\u05DC\u05DA \u05D5\u05D4\u05E8\u05E5 `bundle install`. \u05DC\u05D0\u05D7\
  \u05E8 \u05DE\u05DB\u05DF, \u05E9\u05D7\u05D5\u05E7 \u05D0\u05EA `byebug` \u05D1\
  \u05D3\u05D9\u05D5\u05E7 \u05D1\u05DE\u05E7\u05D5\u05DD \u05D1\u05D5\u2026"
lastmod: '2024-03-13T22:44:40.212055-06:00'
model: gpt-4-0125-preview
summary: "\u05E8\u05D5\u05D1\u05D9 \u05DE\u05D2\u05D9\u05E2\u05D4 \u05E2\u05DD \u05DE\
  \u05E0\u05E4\u05D4 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\
  \u05D4 \u05D1\u05E9\u05DD `byebug`. \u05E8\u05D0\u05E9\u05D9\u05EA, \u05DB\u05DC\
  \u05D5\u05DC \u05D0\u05EA `byebug` \u05D1\u05E7\u05D5\u05D1\u05E5 Gemfile \u05E9\
  \u05DC\u05DA \u05D5\u05D4\u05E8\u05E5 `bundle install`. \u05DC\u05D0\u05D7\u05E8\
  \ \u05DE\u05DB\u05DF, \u05E9\u05D7\u05D5\u05E7 \u05D0\u05EA `byebug` \u05D1\u05D3\
  \u05D9\u05D5\u05E7 \u05D1\u05DE\u05E7\u05D5\u05DD \u05D1\u05D5\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8"
weight: 35
---

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
