---
title:                "קריאת פרמטרים משורת הפקודה"
aliases:
- he/ruby/reading-command-line-arguments.md
date:                  2024-01-20T17:57:11.563859-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא לקחת מידע שהמשתמש מזין כחלק מהפקודה להפעלת התוכנית. תכניתנים עושים זאת כדי לאפשר התאמה דינמית של התוכנית בעת הרצתה ללא צורך בשינויי קוד.

## איך לעשות:
```Ruby
# בדוגמא זו התוכנית מדפיסה את כל הארגומנטים שהועברו אליה
ARGV.each do |arg|
  puts arg
end
```
פלט לדוגמא, אם סקריפט זה יופעל עם הפקודה `ruby script.rb תפוח משחק ספר`:
```
תפוח
משחק
ספר
```
## עיון נוסף:
היסטורית, ARGV זו הדרך המקובלת לקריאת ארגומנטים ברוב השפות לכתיבת סקריפטים. זה מאפשר לסקריפטים להיות גמישים ומתאימים למגוון צרכים. חלופות כוללות שימוש בספריות חיצוניות כמו `OptionParser` או `Thor`, שמציעות ממשק נוח יותר לעיבוד אופציות ופלגים. כאשר מעבירים ארגומנטים לתוכנית, רובי מכניס אותם למערך ה-ARGV, ואז אפשר לגשת אליו מאיפה שתרצה בקוד.

## ראה גם:
- [תיעוד Ruby לקריאת ארגומנטים משורת הפקודה](https://www.rubyguides.com/2018/12/ruby-argv/)
- [OptionParser](https://ruby-doc.org/stdlib-2.6.1/libdoc/optparse/rdoc/OptionParser.html)
- [Thor](http://whatisthor.com/)
