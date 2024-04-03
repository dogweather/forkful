---
date: 2024-01-26 03:38:18.569710-07:00
description: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\
  \u05D1 \u05E7\u05D9\u05D9\u05DD \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9\
  \ \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\
  \u05E0\u05D9\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\
  \u05E2\u05D9\u05DD \u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2\
  \ \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E9\u05E4\u05E8 \u05D0\u05EA \u05D4\u05EA\
  \u05DB\u05D5\u05E0\u05D5\u05EA \u05D4\u05DC\u05D0 \u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05E9\u05DC\u2026"
lastmod: '2024-03-13T22:44:40.218700-06:00'
model: gpt-4-0125-preview
summary: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\
  \u05D1 \u05E7\u05D9\u05D9\u05DD \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9\
  \ \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\
  \u05E0\u05D9\u05EA."
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
weight: 19
---

## איך לבצע:
בואו נעבור על דוגמה של ריפקטורינג למתודה ברובי שחושבת את סכום הריבועים.

**לפני ריפקטורינג:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # פלט: 14
```

**אחרי ריפקטורינג:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # פלט: 14
```

הגרסה המרופקטרת משתמשת בEnumerable של רובי כדי לבטא את אותה הלוגיקה בצורה תמציתית וברורה יותר. המתודה `map` משנה כל אלמנט, ו-`sum` מצטבר את ערכיהם, מה שמסיר את הצורך בניהול לולאה ידנית והשמת משתנים.

## צלילה עמוקה
לריפקטורינג יש הקשר היסטורי עשיר, שחוזר אחורה לתרגולים המוקדמים בפיתוח תוכנה. התייחסויות ראשונות ניתן לעקוב אחריהן חזרה לשנות ה-90, עם תרומות נכבדות שנעשו על ידי מרטין פאולר בספרו "Refactoring: Improving the Design of Existing Code", שם הוא מספק קטלוג של דפוסים לריפקטורינג. מאז, הריפקטורינג הפך לאבן פינה בתרגולי פיתוח זריז.

כאשר אנו מדברים על אלטרנטיבות לריפקטורינג, עלינו לשקול גישה שונה כמו 'שכתוב', שם תחליף את המערכת הישנה בחלקים או במלואה, או להתאמן בתרגולים כמו 'בדיקות קוד' ו'תכנות זוגי' כדי לשפר בהדרגה את איכות הקוד. עם זאת, אלו אינם תחליפים לריפקטורינג; הם משלימים את התהליך.

מבחינת יישום, רובי מספקת תחביר מצוין וביטויי שלעיתים קרובות גורם לקוד להיות קצר וקריא יותר לאחר ריפקטורינג. העקרונות המרכזיים כוללים DRY (Don't Repeat Yourself - אל תחזור על עצמך), שימוש בשמות משמעותיים, השמת דגש על מתודות קצרות המתמקדות במשימה יחידה, והשימוש במודול ה-Enumerable של רובי באופן יעיל, כפי שנראה בדוגמה למעלה. כלים אוטומטיים כמו RuboCop יכולים גם לעזור למתכנתים לזהות נקודות בקוד שיכולות להרוויח מריפקטורינג.

## ראה גם
לחקור עוד על ריפקטורינג ברובי, בדוק את המשאבים האלה:

- הספר המכונן של מרטין פאולר: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- המדריך לסגנון של רובי לכתיבת קוד נקי יותר: [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, ניתוח קוד סטטי (linter) ומעצב: [RuboCop GitHub Repository](https://github.com/rubocop/rubocop)
