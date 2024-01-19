---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים המתאימים לתבנית היא תרגיל שמטרתו למחוק תווים מסוימים מתוך מחרוזת. בעידן המידע והנתונים, התרגיל הזה הופך להיות בלתי נפסק, כדי לנקות נתונים, לרפורמט מידע או לבצע עיבוד טקסט.

## איך לעשות:
בשפת התכנות Ruby, ניתן להשתמש בשיטה `.delete`. היא ממחקת כל תו שמתאים לתבנית. נראה כיצד ניתן למחוק את כל האותיות הקטנות מתוך מחרוזת:

```ruby
str = "Hello there, my friend!"
new_str = str.delete "a-z"
puts new_str
# "H, !"
```

## בעומק:
על פי בנייתה של Ruby, `.delete` מחזירה עותק של המחרודת, כאשר כל תו המתאים לתבנית מוחק. אם אתם זקוקים לשקול אופציות אחרות, אתם יכולים להשתמש בשיטה `.gsub`, שמאפשרת מחיקת תווים שמתאימים לביטוי רגולרי. אך שימו לב, `.gsub` דורשת יותר משאבים מ-`.delete`, אז תשקלו איפה ומתי להשתמש.

```ruby
str = "Hello there, my friend!"
new_str = str.gsub /[a-z]/, ''
puts new_str
# "H, !"
```

## ראה גם:
- [מתכנת ה-Ruby: מחיקת תווים ממחרוזת](https://www.rubyguides.com/2018/10/ruby-string-methods/#delete-method)
- [תיעוד שיטה `.delete`](https://ruby-doc.org/core-2.6.1/String.html#method-i-delete)
- [תיעוד שיטה `.gsub`](https://ruby-doc.org/core-2.6.1/String.html#method-i-gsub)