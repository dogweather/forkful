---
title:    "Ruby: השוואת שתי תאריכים"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# למה:
ישנם מקרים רבים בתכנות בשפת Ruby שמבקשים להשוות בין שתי תאריכים. הקוד הבא יטפל במהלך השוואה ויענה על השאלה של "איך להשוות שני תאריכים".

## איך לעשות:
ההשוואה בין תאריכים בשפת Ruby יכולה להיות מורכבת, אך ישנם כמה דרכים לבצע אותה. הקוד הבא מדגים שימוש במערך של שני תאריכים ומדפיס את התאריכים התואמים. התוצאה הסופית תחזיק את התאריכים המשותפים ביניהם.

```Ruby
require 'date'

array = ["2019-01-01","2019-02-15","2019-03-14","2019-04-01"]

common_dates = array.map { |date| Date.parse(date) }
common_dates.keep_if { |date| common_dates.count(date) > 1 }

puts common_dates 
```

כתוצאה מהקוד הנ"ל, התוצאה תהיה:

```Ruby
2019-01-01
```

אם ברצונך להשוות בין שני תאריכים בפורמט של מחרוזת, ניתן להשתמש בפונקציה `Date.parse()` כדי להמיר את התאריך למחרוזת תקפה ולאחר מכן להשוות ביניהם כמו במשל הבא:

```Ruby
today = Date.today.strftime("%Y-%m-%d")
tomorrow = (Date.today + 1).strftime("%Y-%m-%d")

if today == tomorrow
  puts "היום ומחר זה אותו תאריך!"
else
  puts "היום ומחר הם תאריכים שונים"
end
```

## עומק:
למרבה המזל, ישנם מספר פונקציות בשפת Ruby שמסייעות בהשוואת תאריכים בצורה מתקדמת יותר. כאשר נכתוב `Date.today < Date.tomorrow` נקבל ערך `true` מציין שהתאריך היום הוא קטן מהתאריך מחר. ניתן גם להשוות בין תאריכים בשימוש בפונקציות כמו `next_week` ו-`next_month` כדי לקבוע קבוצות של התאריכים שונים על פי תנאים מסוימים.

# ראה גם:
- [מדריך לתאריכים ושעות ב-Ruby](https://www.rubyguides.com