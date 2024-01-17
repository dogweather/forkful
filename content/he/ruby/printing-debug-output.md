---
title:                "הדפסת פלטות ניטור שגיאות"
html_title:           "Ruby: הדפסת פלטות ניטור שגיאות"
simple_title:         "הדפסת פלטות ניטור שגיאות"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

נדונים בפלט נכון: למה ואיך
 כשאתם כותבים קוד, אתם כנראה תתקלו בבעיות שאתם חייבים לפתור. לפעמים זה קשה לעקוב אחר הקוד כי אתם לא יודעים מה הולך בשגיאה כאשר כל מה שאתם רואים זה מספרים ותווים בטקסט. נדון בפלט, יש מידע נתמך שתוכלו להשתמש בו על מנת לבחון את המשתמשים של האפליקציה ולזהות את הבעיות של הקוד שלכם.

איך להגדיר פלט נכון:

```Ruby
def print_debug_output
  puts "Debug information"
  puts "More debug info"
  puts "Even more debug info"
end
```

פלט:
```
Debug information
More debug info
Even more debug info
```
הערה: פלט נכון זה הודעות שנכתבו בדיוק ושהן מציגות את המידע שאתם צריכים לראות.

 

עומק מתחכם:

על רוצח נשקיים, כזה שמדביק לכם פירות באפליקציה שלכם זהה בעיקר להמבחן שעוד מתחילים לבחון. אם אתה מתקבל לפירמית קוד הזן, אתה יכול להשתמש במעבר כדי להפנות את המעמד שלך ולספר לך כמה רבות את האוסמה במספר מקרים אתה תן להם.
היום, תחום זה התפתח מאוד מאוד,
השיטה הפופולרית ביותר לנרדור את הכיוון.
על כמה את תקבל שדוונים פס וצבעים בנושא.

ראה גם:

- [כתבות Debugging עבור פייתון מתוך נתונים](http://www.pagetophr.org/articles/debugging-introduction)
- [שיטות ניתוח שירותי debug פייתון עם ddwrit](https://www.codingmamad.com/2018/06/20/python-debug-mode-with-ddwrit/)