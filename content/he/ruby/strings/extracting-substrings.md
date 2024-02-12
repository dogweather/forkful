---
title:                "חילוץ תת-מחרוזות"
aliases: - /he/ruby/extracting-substrings.md
date:                  2024-01-20T17:46:43.020249-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# מה ולמה?
חילוץ תת-מחרוזות הוא לקיחת חלק ממחרוזת אחת ויצירת מחרוזת חדשה ממנה. מתכנתים עושים זאת כדי לאסוף נתונים ספציפיים, לנתח טקסט או לשנות פורמטים.

# איך לעשות:
זה הקוד:
```Ruby
# לקיחת תת-מחרוזת בעזרת טווחי מיקומים (indices)
str = "שלום עולם!"
substring = str[6, 5] # תחילה המיקום, אחר כך האורך
puts substring
# => עולם!

# לקיחת תת-מחרוזת בעזרת ריינג' (Range)
substring = str[6..10]
puts substring
# => עולם!
```

# טבילה עמוקה
מיקוח על חילוץ תת-מחרוזות היה קיים מזמן בתכנות; שיטות שונות כמו רגקס (Regex) או פעולות מחרוזת בסיסיות משמשות מתכנתים לשם כך. הריינג' ברובי נועד לספק גמישות בבחירת חלקי מחרוזת. קוד אופטימלי צריך להיות ברור ולהימנע מלכלול חישובים מסובכים שקשה להבין או לתחזק.

# ראה גם
- [Ruby String Documentation](https://ruby-doc.org/core/String.html)
- [שימוש ב-Regular Expressions ברובי](https://ruby-doc.org/core/Regexp.html)
- [מדריך לשימוש ב-Range ברובי](https://ruby-doc.org/core/Range.html)
