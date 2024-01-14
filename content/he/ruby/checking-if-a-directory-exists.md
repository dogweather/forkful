---
title:                "Ruby: בדיקת קיום תיקייה במחשב"
simple_title:         "בדיקת קיום תיקייה במחשב"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מדוע

ביצוע בדיקה אם תיקייה קיימת בשפה רובי יכול להיות חשוב כאשר פועלים עם קבצים ומידע שמאוחסן בתיקיות. כאשר תיקייה לא קיימת, זה יכול לייצב את תהליך הפיתוח ולהקל על אתר מסוים.

## איך לבצע בדיקה אם תיקייה קיימת

הוראות קוד ופלט דוגמה בתוך קטעים קוד "```Ruby ... ```":

```Ruby
# הכנת תיקייה חדשה לבדיקה
directory = "new_directory"

# בדיקה אם התיקייה קיימת
if File.directory?(directory)
  puts "#{directory} קיימת."
else
  puts "#{directory} לא קיימת."
end
```
פלט:
```
new_directory לא קיימת.
```
בדיקה אם תיקייה קיימת ניתן גם לבצע עם פונקציות נוספות כמו `Dir.exist?` ו- `File.exist?`.

## לנוסוף לעומק על בדיקת קיומה של תיקייה

בנוסף לשימוש הבסיסי שלה לבדיקת תיקייה, ישנם עוד כמה נסיבות שבהן יכולה להיות נחמד לבדוק אם תיקייה קיימת.

למשל, במידה ונרצה לאמת אם תיקייה קיימת תחת תיקיית מסוימת, ניתן להשתמש בפונקציה `Dir.exist?` יחד עם שם התיקייה הרלוונטית כניסת הפרמטר.

בנוסף, ניתן גם להשתמש במתודה `exists?` לבדיקה אם תיקייה או קובץ קיימים בפעם אחת על ידי שימוש בתכונה `joined` על ידי תיקייה או קובץ.

## ראה גם

- [כיצד ליצור תיקייה חדשה ברובי](https://www.rubyguides.com/2021/01/create-directory-ruby/)
- [המחברת הרדופה של תיקייה קיימת ב-Ruby](https://blog.honeybadger.io/how-to-detect-if-a-directory-exists-in-ruby/)