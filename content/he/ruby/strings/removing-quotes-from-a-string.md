---
date: 2024-01-26 03:42:50.887554-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA \u05D6\
  \u05D4: \u05DC-Ruby \u05D9\u05E9 \u05DB\u05DE\u05D4 \u05D8\u05E8\u05D9\u05E7\u05D9\
  \u05DD \u05E0\u05D7\u05DE\u05D3\u05D9\u05DD \u05D1\u05E9\u05E8\u05D5\u05D5\u05DC\
  \ \u05E9\u05DC\u05D4 \u05DC\u05E7\u05D9\u05E6\u05D5\u05E5 \u05E1\u05D9\u05DE\u05E0\
  \u05D9 \u05D4\u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05D4\u05DC\u05D0 \u05E8\u05E6\
  \u05D5\u05D9\u05D9\u05DD \u05D4\u05DC\u05DC\u05D5. \u05D0\u05EA\u05DD \u05D9\u05DB\
  \u05D5\u05DC\u05D9\u05DD \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E9\u05D9\
  \u05D8\u05D5\u05EA `gsub` \u05D0\u05D5 `delete` \u05DB\u05D3\u05D9 \u05DC\u05D1\u05E6\
  \u05E2 \u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:40.181798-06:00'
model: gpt-4-0125-preview
summary: "\u05DC-Ruby \u05D9\u05E9 \u05DB\u05DE\u05D4 \u05D8\u05E8\u05D9\u05E7\u05D9\
  \u05DD \u05E0\u05D7\u05DE\u05D3\u05D9\u05DD \u05D1\u05E9\u05E8\u05D5\u05D5\u05DC\
  \ \u05E9\u05DC\u05D4 \u05DC\u05E7\u05D9\u05E6\u05D5\u05E5 \u05E1\u05D9\u05DE\u05E0\
  \u05D9 \u05D4\u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05D4\u05DC\u05D0 \u05E8\u05E6\
  \u05D5\u05D9\u05D9\u05DD \u05D4\u05DC\u05DC\u05D5."
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

## איך לעשות את זה:
ל-Ruby יש כמה טריקים נחמדים בשרוול שלה לקיצוץ סימני המרכאות הלא רצויים הללו. אתם יכולים להשתמש בשיטות `gsub` או `delete` כדי לבצע את העבודה. הנה קצת קוד לעיסה:

```ruby
# שימוש ב-gsub כדי להסיר מרכאות כפולות ויחידות
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# פלט: Say hello to my little friend!

# אם אתם יודעים שתתמודדו רק עם סוג אחד של מרכאות
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# פלט: Stay a while and listen!
```

## צלילה עמוקה
ההיסטוריה של המרכאות חוזרת לימי התחלות התכנות, שם לעיתים קרובות הם שימשו כמגבלי מחרוזת. כיום, כפי שהיה אז, ייתכן ותמצאו את עצמכם זקוקים להסרת תווי המרכאות הללו כאשר הם אינם נחוצים או כאשר הם עלולים להתערב באחסון ובעיבוד נתונים.

דיברנו על `gsub` ו-`delete` אבל ישנם גם שיטות נוספות, כמו `tr` או `tr_s`, שנותנות לכם קצת יותר שליטה או יכולות לטפל במקרי שימוש שונים:

```ruby
# tr גם יכול להסיר מרכאות
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# פלט: Do or do not, there is no try.
```

זכרו, לכל אחת מהשיטות האלה ישנן מקרי שימוש. `gsub` חזק יותר כאשר אתם מתעמתים עם דפוסים מורכבים או החלפות מרובות. `delete` ו-`tr` עובדות יפה ביותר להסרה פשוטה וישירה של תווים.

## ראו גם
לקריאה נוספת, ולראות את השיטות האלו בפעולה בתוך קודבייסים גדולים יותר, בדקו:
- התיעוד של Ruby עבור [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), ו-[String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- Ruby Monstas יש סט תרגילים מעולה עבור [מחרוזות](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html), שכולל עבודה עם מרכאות.
- דיונים ב-Stack Overflow על [ניהול מחרוזות](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) מספקים בעיות ופתרונות מהעולם האמיתי מחברי Ruby.
