---
title:                "Ruby: קריאת ארגומנטים מקו הפקודה"
simple_title:         "קריאת ארגומנטים מקו הפקודה"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

מדוע: כיוון שקריאת הארגומנטים של שורת הפקודה היא כלי מאוד חשוב בפיתוח בשפת רובי, משקל התבוננות במושג זה חשוב בכדי להתקדם כמפתח רובי.

כיצד לקרוא ארגומנטים: 
```Ruby 
ARGV.each do |arg|
  puts "הארגומנט הוא #{arg}"
end
```

כאן אנו מריצים את הקוד בעזרת פקודת רובי "ruby script.rb argument1 argument2" ונראה את הפלט הבא: 
```
הארגומנט הוא argument1
הארגומנט הוא argument2
```

מעמיקים עוד קצת: איך ניתן לעקוב אחר הארגומנטים של שורת הפקודה? ובעזרת איזה כלים? למה זה חשוב בכלל? הנה כמה טיפים שיעזרו לך בכתיבת קוד עם שימוש בארגומנטים של שורת הפקודה:

1. ניתן לקבל את כמות הארגומנטים באמצעות פקודת "length": 
```Ruby
puts ARGV.length
```
במקרה זה, אם נריץ את הקוד עם 3 ארגומנטים, נקבל את הפלט "3".

2. אם נרצה להיפטר מהאם הארגומנטים נמצאים בגרש או לא, ניתן להשתמש בפקודת "start_with?": 
```Ruby
puts ARGV[0].start_with?("-")
```
במקרה של הארגומנט "argument1", נקבל את הפלט "false".

רוצים לשפר עוד יותר את הקוד? אז ניתן להשתמש בספריית אופטיונס עם מודול 'optparse'. תוכלו ללמוד עוד עליה בקישורים המצורפים למטה.

ראו גם:
- [טיפים ואסכולות לקריאת פרמטרים של שורת הפקודה ברובי מאת "itamarst"](https://www.rubyguides.com/2018/03/ruby-argv/)
- [הסבר מפורט על שימוש בשורת הפקודה בפרוייקטי רובי מאת "avdi"](https://avdi.codes/command-line-arguments-for-sexy-projects/)
- [מדריך