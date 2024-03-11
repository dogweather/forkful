---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:37.437229-07:00
description: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA\
  \ \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4 \u05DC\
  \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\
  \u05D5\u05DF \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\
  \ \u05D2\u05D3\u05D5\u05DC\u05D4 \u05D5\u05D0\u05EA \u05E9\u05D0\u05E8 \u05D4\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\
  \u05D8\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DE\u05E1\u05D9\u05D1\u05D5\u05EA \u05DB\u05DE\
  \u05D5 \u05D4\u05EA\u05D0\u05DE\u05D4 \u05DC\u05DE\u05D5\u05E1\u05DB\u05DE\u05D5\
  \u05EA\u2026"
lastmod: '2024-03-11T00:14:13.683249-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA \u05D1\
  \u05EA\u05DB\u05E0\u05D5\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4 \u05DC\u05D4\
  \u05DE\u05E8\u05D4 \u05E9\u05DC \u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\
  \u05DF \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\
  \u05D3\u05D5\u05DC\u05D4 \u05D5\u05D0\u05EA \u05E9\u05D0\u05E8 \u05D4\u05D0\u05D5\
  \u05EA\u05D9\u05D5\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\
  \u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DE\u05E1\u05D9\u05D1\u05D5\u05EA \u05DB\u05DE\u05D5\
  \ \u05D4\u05EA\u05D0\u05DE\u05D4 \u05DC\u05DE\u05D5\u05E1\u05DB\u05DE\u05D5\u05EA\
  \u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות ראשיות בתכנות פירושה להמרה של התו הראשון במחרוזת לאות גדולה ואת שאר האותיות לאותיות קטנות. תכנתים עושים זאת מסיבות כמו התאמה למוסכמות ניסוח, הפיכת הפלטים לקריאים יותר, או הבטחת עקביות נתונים לצורך השוואות ואחסון.

## איך לעשות זאת:
Ruby מספקת שיטות ישירות למניפולציית מחרוזות, כולל הפיכה לאותיות ראשיות. הנה איך אפשר להפוך מחרוזת לאותיות ראשיות ב-Ruby:

```ruby
# שיטה מובנית של Ruby
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

שיטת ה`.capitalize` של Ruby נוחה אבל משפיעה רק על האות הראשונה. לשליטה רבה יותר או להפיכת כל מילה במחרוזת לאות ראשית (הידועה כ-case כותרת), ייתכן שתרצה להשתמש בשיטת ה`titleize` מהרחבת ActiveSupport של Rails, או ליישם אותה בעצמך:

```ruby
# שימוש ב'titleize' של ActiveSupport ב-Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

אם אינך משתמש ב-Rails או מעדיף פתרון רובי טהור, הנה איך תוכל להפוך כל מילה במחרוזת לאות ראשית:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

שיטה זו מחלקת את המחרוזת למערך של מילים, מרימה את כל אחת מהן לאות ראשית, ולאחר מכן מחברת אותן בחזרה יחדיו בעזרת רווח.
