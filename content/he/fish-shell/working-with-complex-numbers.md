---
title:                "עבודה עם מספרים מרוכבים"
aliases:
- he/fish-shell/working-with-complex-numbers.md
date:                  2024-01-26T04:40:52.395495-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מרוכבים מרחיבים את הרעיון של קווי מספרים חד-ממדיים למישור מרוכב דו-ממדי. מתכנתים משתמשים בהם בתחומים כמו הנדסה, פיזיקה, וגרפיקה לחישובים שדורשים שני רכיבים, כמו אותות או סיבובים.

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
