---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הופכים מחרוזת לאותיות גדולות כשרוצים שהטקסט יהיה גלוי ובולט יותר. זה שימושי בעיקר לכותרות או כשמובילים לדבר מרכזי.

## איך לעשות:
ב-Ruby, כשרוצים להפוך תווים לאותיות גדולות משתמשים ב-methods כמו `.upcase` או `.capitalize`. נתחיל עם `.capitalize`:

```ruby
puts "shalom".capitalize
# Output: Shalom
```

עכשיו `.upcase`:

```ruby
puts "shalom".upcase
# Output: SHALOM
```

`.capitalize` מגדיל רק את האות הראשונה, בעוד `.upcase` מגדיל הכל.

## צלילה לעומק:
בעברית, עניין ההפיכה לאותיות גדולות פחות רלוונטי, אבל באנגלית זו פעולה נפוצה. שיטות כמו `.upcase` ו`.capitalize` זמינות כבר מהתחלה ב-Ruby. אלטרנטיבה פחות ידועה היא `.swapcase` שמחליפה בין גדולות לקטנות.

```ruby
puts "Shalom".swapcase
# Output: sHALOM
```

מבחינת היישום, Ruby משתמש בטבלת ASCII או Unicode כדי להפוך אותיות קטנות לגדולות ולהפך. הפעולה הזו אינה משנה את המחרוזת המקורית אלא יוצרת מחרוזת חדשה.

## ראה גם:
- תיעוד Ruby למתודות של מחרוזת: [Ruby String Documentation](https://ruby-doc.org/core-3.1.2/String.html)
- פורום תכנות Ruby עבור שאלות ותמיכה: [Ruby Forum](https://www.ruby-forum.com/)
