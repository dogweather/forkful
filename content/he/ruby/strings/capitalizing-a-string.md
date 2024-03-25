---
title:                "הפיכת מחרוזת לאותיות גדולות"
date:                  2024-03-25T17:32:21.737099-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
הון המילה במחרוזת זה לרוב אומר להמיר את התו הראשון במחרוזת לאות רישית ואת השאר לאותיות קטנות. אבל לעיתים זה יכול גם להוביל רק לוודא שהתו הראשון הוא באות רישית תוך השארת שאר המחרוזת ללא שינוי. באמת, לדעתי, זה מונח די מעורפל.

## איך לעשות:
רובי מספקת [שיטות ישירות לניפוי מחרוזות](https://docs.ruby-lang.org/en/3.3/String.html), כולל הון המילה:

```ruby
# פונקציה מובנית ברובי
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

נוח מאוד.

המתודה `.capitalize` של רובי נוחה, אך מהפכת את האות הראשונה לאות רישית בלבד. לשליטה רבה יותר או כדי להון כל מילה במחרוזת (מוכר גם כמקרה הכותרת), אולי תרצו להשתמש במתודה `titleize` מרחבת התוספות של Rails ActiveSupport, או ליישם זאת בעצמכם:

```ruby
# שימוש ב-'titleize' מ-ActiveSupport ב-Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# פתרון עשוי בבית
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

המתודה הזו פוצלת את המחרוזת למערך של מילים, מונה כל אחת מהן, ולאחר מכן מצרפת אותן בחזרה יחד עם רווח.

אישית, אני לוקח את הרעיון הזה הרבה יותר רחוק בקוד שלי. כתבתי את המתודה [`titleize` שלי שכוללת בחשבון מילים קטנות כמו "a" ו-"the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
