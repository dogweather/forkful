---
title:                "כתיבה לשגיאת תקן"
html_title:           "Ruby: כתיבה לשגיאת תקן"
simple_title:         "כתיבה לשגיאת תקן"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##למה

כתיבה להוציא למסך השגיאות המקובל מסייעת בזיהוי ותיקון שגיאות בתוכנית שכתבנו. ניתן להשתמש בה במקרים שבהם רוצים לראות מידע נוסף על מצבי פיתוח או באינטראקציה של משתמשים עם התוכנית.

##כיצד להשתמש

שימוש בפקודה `STDERR.puts` מאפשר לנו להדפיס מידע ישירות למסך השגיאות. לדוגמא: 
```Ruby
STDERR.puts "זוהי הודעת שגיאה"
```
כדי להדפיס את המספר האחרון של השגיאות בקובץ, ניתן להשתמש במשתנה `STDERR.fileno` ולשלוח אותו כארגומנט לפקודת `set_backtrace`. לדוגמא:
```Ruby
begin
  raise RuntimeError
rescue
  STDERR.puts "מספר השגיאות: #{STDERR.fileno}"
end
```
פלט יחיד:
```
מספר השגיאות: 2
```

##עיון מעמיק

כאשר נרצה להדפיס מידע מפורט יותר על שגיאה מסוימת, ניתן לשלוח אובייקט שגיאה כארגומנט לפקודת `set_backtrace`. לדוגמא: 
```Ruby
begin
  1/0
rescue => e
  STDERR.puts e.backtrace
end
```
פלט יחיד:
```
תקריב "divide by 0"
משלוח טופס מתוך /ruby/runs/dive.rb:2:in `<main>'
```

##ראו גם

- [מאמר על כתיבה לפלט הסטנדרטי בשפת Ruby] (https://www.toptal.com/ruby/ruby-error-handling-and-exceptions) 
- [מדריך על מערכת השגיאות בשפת Ruby] (https://www.rubyguides.com/2019/07/ruby-exceptions/)