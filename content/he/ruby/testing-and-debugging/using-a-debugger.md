---
aliases:
- /he/ruby/using-a-debugger/
date: 2024-01-26 04:10:30.635726-07:00
description: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA (debugger) \u05D1\u05E8\u05D5\u05D1\u05D9 \u05DE\u05E2\
  \u05E0\u05D9\u05E7 \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DB\u05D5\
  \u05D7-\u05E2\u05DC \u05DC\u05D4\u05E9\u05D4\u05D5\u05EA \u05D0\u05EA \u05D4\u05E7\
  \u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD, \u05DC\u05D1\u05D7\u05D5\u05DF \u05DE\u05E9\
  \u05EA\u05E0\u05D9\u05DD \u05D5\u05DC\u05E2\u05D1\u05D5\u05E8 \u05E2\u05DC \u05D4\
  \u05E7\u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD \u05E9\u05D5\u05E8\u05D4 \u05D0\u05D7\
  \u05E8 \u05E9\u05D5\u05E8\u05D4. \u05D0\u05E0\u05E9\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D3\u05DB\u05D0\u2026"
lastmod: 2024-02-18 23:08:53.395756
model: gpt-4-0125-preview
summary: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA (debugger) \u05D1\u05E8\u05D5\u05D1\u05D9 \u05DE\u05E2\u05E0\
  \u05D9\u05E7 \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DB\u05D5\u05D7\
  -\u05E2\u05DC \u05DC\u05D4\u05E9\u05D4\u05D5\u05EA \u05D0\u05EA \u05D4\u05E7\u05D5\
  \u05D3 \u05E9\u05DC\u05D4\u05DD, \u05DC\u05D1\u05D7\u05D5\u05DF \u05DE\u05E9\u05EA\
  \u05E0\u05D9\u05DD \u05D5\u05DC\u05E2\u05D1\u05D5\u05E8 \u05E2\u05DC \u05D4\u05E7\
  \u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD \u05E9\u05D5\u05E8\u05D4 \u05D0\u05D7\u05E8\
  \ \u05E9\u05D5\u05E8\u05D4. \u05D0\u05E0\u05E9\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D3\u05DB\u05D0\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8"
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
