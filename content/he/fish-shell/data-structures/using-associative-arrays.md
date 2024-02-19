---
aliases:
- /he/fish-shell/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:25.600600-07:00
description: "\u05DE\u05E2\u05E8\u05DB\u05D9 \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05E6\
  \u05D9\u05D5\u05EA, \u05D0\u05D5 \u05DE\u05E4\u05D5\u05EA \u05D2\u05D9\u05E9\u05D4\
  , \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05DA \u05DC\u05D0\u05D7\u05E1\
  \u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DB\u05D6\u05D5\u05D2\u05D5\u05EA\
  \ \u05DE\u05E4\u05EA\u05D7-\u05E2\u05E8\u05DA, \u05D3\u05D1\u05E8 \u05D4\u05DE\u05E7\
  \u05DC \u05E2\u05DC \u05D4\u05D0\u05E8\u05D2\u05D5\u05DF \u05D5\u05D4\u05D0\u05D7\
  \u05D6\u05D5\u05E8 \u05E9\u05DC \u05DE\u05D9\u05D3\u05E2 \u05DC\u05E4\u05D9 \u05DE\
  \u05E4\u05EA\u05D7. \u05D4\u05DD \u05DE\u05D0\u05D5\u05D3 \u05E9\u05D9\u05DE\u05D5\
  \u05E9\u05D9\u05D9\u05DD \u05DB\u05D0\u05E9\u05E8 \u05D9\u05E9 \u05E6\u05D5\u05E8\
  \u05DA \u05D1\u05D3\u05E8\u05DA\u2026"
lastmod: 2024-02-18 23:08:53.285337
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05E8\u05DB\u05D9 \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05E6\
  \u05D9\u05D5\u05EA, \u05D0\u05D5 \u05DE\u05E4\u05D5\u05EA \u05D2\u05D9\u05E9\u05D4\
  , \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05DA \u05DC\u05D0\u05D7\u05E1\
  \u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DB\u05D6\u05D5\u05D2\u05D5\u05EA\
  \ \u05DE\u05E4\u05EA\u05D7-\u05E2\u05E8\u05DA, \u05D3\u05D1\u05E8 \u05D4\u05DE\u05E7\
  \u05DC \u05E2\u05DC \u05D4\u05D0\u05E8\u05D2\u05D5\u05DF \u05D5\u05D4\u05D0\u05D7\
  \u05D6\u05D5\u05E8 \u05E9\u05DC \u05DE\u05D9\u05D3\u05E2 \u05DC\u05E4\u05D9 \u05DE\
  \u05E4\u05EA\u05D7. \u05D4\u05DD \u05DE\u05D0\u05D5\u05D3 \u05E9\u05D9\u05DE\u05D5\
  \u05E9\u05D9\u05D9\u05DD \u05DB\u05D0\u05E9\u05E8 \u05D9\u05E9 \u05E6\u05D5\u05E8\
  \u05DA \u05D1\u05D3\u05E8\u05DA\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

מערכי אסוציאציות, או מפות גישה, מאפשרים לך לאחסן נתונים כזוגות מפתח-ערך, דבר המקל על הארגון והאחזור של מידע לפי מפתח. הם מאוד שימושיים כאשר יש צורך בדרך מובנית יותר להתמודד עם נתונים לעומת רשימות בלבד, במיוחד בהגדרות ובעת טיפול במגוון אטריביוטים.

## איך ל:

Fish אינו תומך באופן ילידי במערכי אסוציאציה כמו Bash 4+, אך ניתן להשיג פונקציונליות דומה באמצעות שילוב של רשימות ומניפולציות מחרוזות. כך תוכלו לחקות אותם:

ראשית, התקנת אלמנטים של "מערך אסוציאטיבי" בנפרד:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

לגישה לאלמנט, פשוט התייחס אליו ישירות:

```Fish Shell
echo $food_color_apple
# פלט: red
```

אם יש צורך לשלוף אותם בלולאה, השתמש בלולאת for תוך שימוש בקונבנציה של שמות:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# פלט:
# red
# yellow
```

למי שחסר Bash בלתי `${!array[@]}` לקבלת כל המפתחות, ניתן לאחסן מפתחות ברשימה נפרדת:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'הוא' $food_color_$key
end
# פלט:
# apple הוא red
# banana הוא yellow
```

## צלילה עמוקה

מערכי אסוציאציה אמיתיים כפי שקיימים בשפות תסריט אחרות עדיין אינם חלק מהגישה של Fish. הפתרון המוצע מנצל את יכולות המניפולציה של מחרוזות והרשימות של Fish על מנת ליצור מבנה דמוי מערך אסוציאטיבי. אף על פי שזה עובד, זה לא נקי או חסין לשגיאות כמו שתמיכה מובנית במערכי אסוציאציה הייתה נראית. מעטפות שלל כמו Bash ו-Zsh כוללות תמיכה מובנית במערכי אסוציאציה, מה שמוביל לקוד יותר ישיר וקריא. עם זאת, פילוסופיית העיצוב של Fish שואפת לפשטות וידידותיות למשתמש, אולי על חשבון תכונות כאלה. הפתרון מספק מענה לרוב הצרכים אך חשוב להמשיך ולעקוב אחרי התפתחות Shell של Fish - המפתחים שלו משפרים ומוסיפים תכונות באופן פעיל על סמך משוב מהקהילה.
