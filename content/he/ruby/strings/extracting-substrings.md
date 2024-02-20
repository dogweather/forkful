---
date: 2024-01-20 17:46:43.020249-07:00
description: null
lastmod: 2024-02-19 22:04:59.460146
model: gpt-4-1106-preview
summary: null
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
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
