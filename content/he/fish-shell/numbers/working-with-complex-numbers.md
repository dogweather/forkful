---
date: 2024-01-26 04:40:52.395495-07:00
description: "\u05D0\u05D9\u05DA \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4: \u05D1-Fish, \u05D0\u05E0\u05D5 \u05DE\u05EA\u05DE\u05D5\u05D3\u05D3\u05D9\
  \u05DD \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\
  \u05D1\u05D9\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA `math` \u05E2\u05DD\
  \ \u05D7\u05DC\u05E7\u05D9\u05DD \u05DE\u05DE\u05E9\u05D9\u05D9\u05DD \u05D5\u05D3\
  \u05DE\u05D9\u05D5\u05E0\u05D9\u05D9\u05DD. \u05D4\u05E0\u05D4 \u05D4\u05EA\u05D7\
  \u05DC\u05D4."
lastmod: '2024-03-13T22:44:40.035016-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Fish, \u05D0\u05E0\u05D5 \u05DE\u05EA\u05DE\u05D5\u05D3\u05D3\u05D9\
  \u05DD \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\
  \u05D1\u05D9\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA `math` \u05E2\u05DD\
  \ \u05D7\u05DC\u05E7\u05D9\u05DD \u05DE\u05DE\u05E9\u05D9\u05D9\u05DD \u05D5\u05D3\
  \u05DE\u05D9\u05D5\u05E0\u05D9\u05D9\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך עושים את זה:
ב-Fish, אנו מתמודדים עם מספרים מרוכבים באמצעות `math` עם חלקים ממשיים ודמיוניים. הנה התחלה:

```fish
# הוספת שני מספרים מרוכבים (3+4i) ו-(5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # מוציא: 8+6i

# כפל שני מספרים מרוכבים (1+2i) ו-(3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # מוציא: -5+10i
```

אם אתם צריכים להעלות מספר מרוכב בחזקה או לקבל את צורתו המעריכית:

```fish
# ריבוע של (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # מוציא: -5+12i

# אקספוננציאל של (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # מוציא: -0.41615+0.9093i
```

## טבילה עמוקה
תמיכת Fish Shell במספרים מרוכבים היא יחסית חדשה, התחילה בערך בגרסה 3.1.0. לפני כן, אנשים אולי השתמשו ב-`bc` או קראו לכלים חיצוניים כמו Python למתמטיקה מורכבת.

חלופות למתן של Fish כוללות ספריות מספריות מיוחדות או שפות כמו MATLAB, Python עם NumPy, או אפילו C++ עם הספרייה התקנית. עם זאת, אלו עשויות להיות יתר על המידה לחישובים מהירים במעטפת.

תמיכת המספרים המרוכבים של Fish משולבת ישירות בפקודת ה-`math` הפנימית שלו, תוך שימוש ב-libcalc. זה אומר שאינכם צריכים להתקין כלים נוספים לפעולות בסיסיות.

עם זאת, Fish אינו מיועד לחישוב מתמטי כבד. יכולת המתמטיקה שלו נוחה לחישובים מהירים או סקריפטים שבהם מספרים מרוכבים נכנסים לתמונה, אך שקלו להשתמש בכלים חזקים יותר למשימות מאומצות.

## ראו גם
- תיעוד Fish shell ל-math: https://fishshell.com/docs/current/commands.html#math
- NumPy ל-Python, חלופה פופולרית: https://numpy.org/
- הבט אמוק יותר לתוך מספרים מרוכבים: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
