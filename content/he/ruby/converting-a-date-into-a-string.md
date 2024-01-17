---
title:                "המרת תאריך למחרוזת"
html_title:           "Ruby: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# מה ולמה?
המרת תאריך למחרוזת היא תהליך שבו משתנה תאריך בתור מחרוזת טקסט. כתיבת תאריך בתור מחרוזת טקסט נחשבת לנוחה יותר עבור מפתחי תוכנה על מנת לטפל בתאריכים בצורה יעילה יותר ולהציגם בצורה מתאימה למשתמשים.

# איך לעשות?
כדי להמיר תאריך למחרוזת בשפת רוובי, ניתן לעשות זאת באמצעות הפקודה `strftime`. הפקודה מאפשרת לנו להציג את התאריך בפורמט מבוקש, כגון חודש/יום/שנה או יום-חודש-שנה. ניתן לראות דוגמאות של הקוד המלא ותוצאות התצוגה בבלוקי קוד `Ruby ... ` להלן:

```Ruby
# תאריך כמחרוזת עם יום השבוע
time = Time.new
puts time.strftime("%A") #=> "יום ראשון"

# תאריך כמחרוזת עם חודש ושנה
time = Time.new
puts time.strftime("%B %Y") #=> "אפריל 2021"
```

# להעמיק
המרת תאריך למחרוזת היא נפוצה בשפות תכנות רבות ונעשית בכדי לייצג תאריכים בצורה נוחה יותר לקריאה ושימוש. ניתן גם להשתמש בתפקיד `strftime` בשפת רוובי לביצוע פעולות נוספות כגון השוואת תאריכים או בניית פורמטים מותאמים לצרכי המשתמש.

למידע נוסף על `strftime` והשימוש בתאריכים בשפת רוובי, ניתן לעיין במקורות המידע המצורפים למטה.

# ראה גם
- [תיעוד על `strftime` בשפת רוובי](https://ruby-doc.org/core-#{RUBY_VERSION}/Time.html#method-i-strftime)
- [המרת תאריך למחרוזת בשפת פייתון](https://www.programiz.com/python-programming/datetime/strftime)
- [המרת תאריך למחרוזת בשפת JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)