---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:59:16.713094-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
חיפוש והחלפת טקסט זה פשוט לומר למחשב "תחפש את זה, ותחליף בזה". מתכנתים עושים את זה כל הזמן – לתקן טעויות, לעדכן קוד, או למצוא דברים במסמכים.

## How to: (איך לעשות:)
רובי מספקת מתודות נוחות לחיפוש והחלפה. זה מסתכם לשימוש ב-`gsub` על מחרוזות:

```Ruby
original_text = "אני אוהב לתכנת ב-Ruby!"
replaced_text = original_text.gsub('Ruby', 'Python')

puts replaced_text
# => אני אוהב לתכנת ב-Python!
```

אפשר גם לחפש עם ביטויים רגולריים:

```Ruby
text_with_typos = "Hello, Worlld!"
corrected_text = text_with_typos.gsub(/Worlld/, 'World')

puts corrected_text
# => Hello, World!
```

## Deep Dive (עומק הנושא):
חיפוש והחלפה היו חלק מתכנות מאז התחלה. כל תוכנה שמעובדת קוד או טקסט דורשת מנגנון כזה. בשפות קודמות היה צורך בלולאות מורכבות ופונקציות מיוחדות, אבל רובי מקלה על החיים עם מתודות מובנות כמו `gsub`.

דרך נוספת היא להשתמש בבלוק:

```Ruby
funny_text = "JavaScript is the best language."
funny_text.gsub(/best/) do |match|
  match == "best" ? "worst" : match
end

puts funny_text
# => JavaScript is the worst language.
```

זה נותן שליטה רבה יותר בהחלפה. ולמען ההגינות, יש שפות נוספות בעלות כלים דומים, אבל רובי זורמת ונעימה במיוחד בשימושים האלה.

## See Also (ראה גם):
- [Ruby Documentation on gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [RegExr - ללמוד ולבדוק ביטויים רגולריים](https://regexr.com/)
- [ֿRuby Style Guide for Regular Expressions](https://rubystyle.guide/#regular-expressions)