---
title:                "הגדלת אותיות במחרוזת"
aliases:
- /he/ruby/capitalizing-a-string/
date:                  2024-02-03T19:06:37.437229-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
