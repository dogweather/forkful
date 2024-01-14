---
title:                "Ruby: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# מדוע
השוואת שתי תאריכים היא כלי חשוב בתכנות של Ruby ויכול להיות שימושי במגוון רחב של תחומי עניין.

## איך לעשות זאת
כדי להשוות שתי תאריכים ב-Ruby, יש להשתמש בפונקציה `compare` ולהעביר את התאריכים בפורמט תקין. לדוגמה:

```
Ruby def compare_dates(date1, date2)     if date1 > date2       puts "#{date1} is after #{date2}"     elsif date1 < date2       puts "#{date1} is before #{date2}"     else       puts "#{date1} and #{date2} are the same"     end   end

compare_dates("2021-01-01", "2022-01-01") # Output: 2022-01-01 is after 2021-01-01
```

## צלילה עמוקה
כאשר משווים שתי תאריכים בעזרת הפונקציה `compare`, המצביע הראשון נותן גבולות של סדר אותיות ושמו השני מצביע גם הוא על סדר הגדולה של התאריכים. תהליך זה יכול להיות מורכב יותר כאשר משווים תאריכים בפורמטים שונים או כאשר משתמשים בפונקציות נוספות כמו `sort`.

# ראו גם
- [רשימת פונקציות של Ruby להשוואת תאריכים](https://ruby-doc.org/core-3.0.1/Date.html#method-i-3C-3D-3E)
- [פורום תמיכה רשמי לפונקציות תאריכים ב-Ruby](https://stackoverflow.com/questions/tagged/ruby+date-functions)
- [מדריך מפורט להשוואת תאריכים ב-Ruby](https://learn.co/lessons/ruby-date-comparison)