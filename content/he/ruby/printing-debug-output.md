---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

דפסת פלט ניפוי שגיאות היא שיטה שבה מפרסמים מידע על קוד בזמן ריצה. מתכנתים משתמשים בהכלים האלה לניפוי באגים ולהבנה משופרת של מהלך התוכנה.

## איך לעשות זאת

ברובי, אנו משתמשים במתודת `puts` לדפיסת פלט של ניפוי שגיאות. הנה בסיס:

```Ruby
def my_func
  x = "Hello, World!"
  puts "Debug: x = #{x}"
end
```

בקוד הזה, ההודעה `"Debug: x = #{x}"` תודפס בעת ריצת הפונקציה `my_func`.

## צלילה עמוקה

**היסטוריה**: בעבר, הצורך להבין את סדר הפעולות של תוכנה גרם למתכנתים להשתמש בטכניקה של דפיסת ניפוי שגיאות.

**חלופות**: ישנם דרכים אחרות לניפוי, כמו הגדרת Breakpoints, Logging, או שימוש ב-Debugger.

**על היישום**: `puts` היא פונקציה ברובי המדפיסה תוצאה למסוף. מגיעה עם גרסאות שונות של גיבוב, אך המשמעות נשארה זהה.

## ראה גם

- [דוקומנטציה](http://ruby-doc.org/core-2.7.0/Kernel.html#method-i-puts)
- [מאמר חמוד בנושא](https://www.honeybadger.io/blog/debugging-ruby-with-puts-print-pp-and-awesome-print/)
- [בלוג מעניין עם טריקים לניפוי שגיאות](https://betterprogramming.pub/5-ways-you-can-debug-your-app-in-ruby-2df8ca72a394)