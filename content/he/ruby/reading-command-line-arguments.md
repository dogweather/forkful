---
title:    "Ruby: קריאת ארגומנטים מהשורת פקודה"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מדוע

קריאת פרמטרים של שורת הפקודה היא כלי חשוב וקריטי בתכנות במקור פתוח של רובי. זה מאפשר למשתמשים להעביר ערכים לתוכנית דרך הפקודה הנכונה להתחברות עם התכנית ולשנות את פעולתה לפי הצורך.

## כיצד לעשות את זה

כדי לקרוא פרמטרים משורת הפקודה ברובי, ניתן להשתמש בפעולת `ARGV`. להלן דוגמה של קוד ופלט:

```ruby
# קובץ לשם קריאת פרמטרים
# קובץ: argv_example.rb

# הפעולה הראשית שתתבצע
def main
  # קורא פרמטרים משורת הפקודה ומשמיט את אינדקס הראשון שהוא השם של הקובץ
  params = ARGV.drop(1)
  puts "הפרמטרים המקובלים הם: #{params}"
end

# קורא את הפונקציה הראשית
main
```

פלט:

```
$ ruby argv_example.rb הפרמטר הראשון הפרמטר השני
הפרמטרים המקובלים הם: ["הפרמטר הראשון", "הפרמטר השני"]
```

## צלילה מעמיקה

כאשר משתמשים עובדים עם קלט משתמש, קריאת פרמטרים של שורת הפקודה היא חיונית כדי לקלט לתוכנית בצורה נוחה ונגישה. ניתן לשלב את זה עם חתימת הפונקציה `optparse` כדי ליצור תצורה גמישה ויעילה של הקלט.

## ראה גם

- [הסבר על קריאת פרמטרים ברובי](https://www.rubyguides.com/2018/12/ruby-command-line-arguments/)
- [מדריך לפעולת `optparse`](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-ruby)
- [תיעוד רשמי על אופציות קריאת פרמטרים ברובי](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)