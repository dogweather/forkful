---
aliases:
- /he/ruby/searching-and-replacing-text/
date: 2024-01-20 17:59:16.713094-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D5\u05DE\
  \u05E8 \u05DC\u05DE\u05D7\u05E9\u05D1 \"\u05EA\u05D7\u05E4\u05E9 \u05D0\u05EA \u05D6\
  \u05D4, \u05D5\u05EA\u05D7\u05DC\u05D9\u05E3 \u05D1\u05D6\u05D4\". \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05DC \u05D4\u05D6\u05DE\u05DF \u2013 \u05DC\u05EA\u05E7\u05DF \u05D8\u05E2\
  \u05D5\u05D9\u05D5\u05EA, \u05DC\u05E2\u05D3\u05DB\u05DF \u05E7\u05D5\u05D3, \u05D0\
  \u05D5 \u05DC\u05DE\u05E6\u05D5\u05D0 \u05D3\u05D1\u05E8\u05D9\u05DD \u05D1\u05DE\
  \u05E1\u05DE\u05DB\u05D9\u05DD."
lastmod: 2024-02-18 23:08:53.373730
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D5\u05DE\u05E8\
  \ \u05DC\u05DE\u05D7\u05E9\u05D1 \"\u05EA\u05D7\u05E4\u05E9 \u05D0\u05EA \u05D6\u05D4\
  , \u05D5\u05EA\u05D7\u05DC\u05D9\u05E3 \u05D1\u05D6\u05D4\". \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\
  \u05DC \u05D4\u05D6\u05DE\u05DF \u2013 \u05DC\u05EA\u05E7\u05DF \u05D8\u05E2\u05D5\
  \u05D9\u05D5\u05EA, \u05DC\u05E2\u05D3\u05DB\u05DF \u05E7\u05D5\u05D3, \u05D0\u05D5\
  \ \u05DC\u05DE\u05E6\u05D5\u05D0 \u05D3\u05D1\u05E8\u05D9\u05DD \u05D1\u05DE\u05E1\
  \u05DE\u05DB\u05D9\u05DD."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
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
