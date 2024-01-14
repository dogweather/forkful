---
title:    "Ruby: מחיקת תווים התואמים לתבנית"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

אנשים עשויים לרצות למחוק תווים התואמים לתבנית על מנת לטפל בתנאים ספציפיים בתוכנית הראשית שלהם.

## כיצד לעשות זאת

```Ruby
# לדוגמה, ביצוע פונקציה שתמחק את האותיות א, ב, ג מתוך רשימת מילים.
words = ["אבק", "בית", "גיל"]
words.delete_if { |word| word.include?("א") || word.include?("ב") || word.include?("ג") }

# פלט: ["ק", "ת"]
```

```Ruby
# אפשר גם להשתמש בתנאי כדי לבדוק כל תו אם הוא נמצא בתבנית
# במקרה זה, נמחק רק את האותיות שהן כפולות.
name = "בובי בראון"
name.delete!("בוב") if name.include?("בוב") # אם תבנית האותיות "בוב" נמצאת בשם
puts name 

# פלט: "יי ראון"
```

## חקירה עמוקה

מחיקת תווים התואמים לתבנית היא טכניקה חזקה ומאוד שימושית כאשר מתמודדים עם מתרחש נאחס בתכנית שלהם. ניתן להשתמש בזה גם כדי לסנן רשימות ולטפל בנתונים עם תבניות מסוימות. זה גם נותן לנו את האפשרות לעבוד עם תבניות כפולות ולמחוק אותן כמו שנרצה.

## ראה גם

- [מדריך לשפת תכנות Ruby](https://www.ruby-lang.org/en/documentation/quickstart/)
- [מדריך עמוק לתכנות Ruby](https://ruby-doc.org/core-2.7.1/)
- [תבניות ותנאים בשפת Ruby](https://www.rubyguides.com/2015/10/ruby-conditions/)