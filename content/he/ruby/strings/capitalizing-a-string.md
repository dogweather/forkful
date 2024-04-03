---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\u05DC\u05D4\u05D2\u05D3\u05D9\u05DC \u05D0\u05D5\u05EA \u05D1\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05D0\
  \u05D5\u05DE\u05E8 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D0\u05EA \u05D4\u05EA\u05D5\
  \ \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05DC\u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05D5\u05D0\u05EA\
  \ \u05E9\u05D0\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05DC\u05D0\u05D5\u05EA\
  \u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05D0\u05DA \u05DC\u05E4\u05E2\
  \u05DE\u05D9\u05DD \u05D6\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05EA\u05DB\
  \u05D5\u05D5\u05DF \u05E8\u05E7 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05EA\
  \u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05D4\u05D5\u05D0\u2026"
lastmod: '2024-03-25T19:22:14.623590-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D4\u05D2\u05D3\u05D9\u05DC \u05D0\u05D5\u05EA \u05D1\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05D0\u05D5\
  \u05DE\u05E8 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D0\u05EA \u05D4\u05EA\u05D5 \u05D4\
  \u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05D5\u05D0\u05EA \u05E9\
  \u05D0\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05DC\u05D0\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA."
title: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D5\u05E0\u05D5\u05EA\
  \ \u05D2\u05D3\u05D5\u05DC\u05D5\u05EA"
weight: 2
---

## איך לעשות זאת:
Ruby מספקת [שיטות ישירות לניפוי מחרוזות](https://docs.ruby-lang.org/en/3.3/String.html), כולל הגדלת אות:

```ruby
# השיטה המובנית של Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

מאוד נוח.

השיטה `.capitalize` של Ruby נוחה אך מגדילה אות ראשית רק לאות הראשונה. לשליטה רבה יותר או להגדלת כל מילה במחרוזת (מה שנקרא כתיב שם פרטי), ייתכן שתרצה להשתמש בשיטה `titleize` מתוספת ה-Rails ActiveSupport, או לממש אותה בעצמך:

```ruby
# שימוש ב-'titleize' של ActiveSupport ב-Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# פתרון עצמאי
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

השיטה הזו מחלקת את המחרוזת למערך של מילים, מגדילה כל אחת מהן, ואז מאחדת אותן יחד עם רווח.

אישית, אני מרחיב את הרעיון הזה הרבה יותר בקוד שלי. כתבתי את השיטה המשלי [`titleize` שמתחשבת במילים קטנות כמו "a" ו-"the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
