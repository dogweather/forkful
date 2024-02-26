---
date: 2024-01-26 04:14:41.658355-07:00
description: "REPL, \u05D0\u05D5 Read-Eval-Print Loop, \u05D4\u05D5\u05D0 \u05E1\u05D1\
  \u05D9\u05D1\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA \u05E9\u05DC\u05D5\u05E7\u05D7\u05EA\
  \ \u05E7\u05DC\u05D8\u05D9\u05DD \u05D1\u05D5\u05D3\u05D3\u05D9\u05DD \u05DE\u05D4\
  \u05DE\u05E9\u05EA\u05DE\u05E9, \u05DE\u05D1\u05E6\u05E2\u05EA \u05D0\u05D5\u05EA\
  \u05DD, \u05D5\u05DE\u05D7\u05D6\u05D9\u05E8\u05D4 \u05D0\u05EA \u05D4\u05EA\u05D5\
  \u05E6\u05D0\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DC\u05DE\u05E9\u05D5\u05D1 \u05DE\u05D9\
  \u05D9\u05D3\u05D9,\u2026"
lastmod: '2024-02-25T18:49:38.294254-07:00'
model: gpt-4-0125-preview
summary: "REPL, \u05D0\u05D5 Read-Eval-Print Loop, \u05D4\u05D5\u05D0 \u05E1\u05D1\
  \u05D9\u05D1\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA \u05E9\u05DC\u05D5\u05E7\u05D7\u05EA\
  \ \u05E7\u05DC\u05D8\u05D9\u05DD \u05D1\u05D5\u05D3\u05D3\u05D9\u05DD \u05DE\u05D4\
  \u05DE\u05E9\u05EA\u05DE\u05E9, \u05DE\u05D1\u05E6\u05E2\u05EA \u05D0\u05D5\u05EA\
  \u05DD, \u05D5\u05DE\u05D7\u05D6\u05D9\u05E8\u05D4 \u05D0\u05EA \u05D4\u05EA\u05D5\
  \u05E6\u05D0\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DC\u05DE\u05E9\u05D5\u05D1 \u05DE\u05D9\
  \u05D9\u05D3\u05D9,\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
---

{{< edit_this_page >}}

## מה ולמה?
REPL, או Read-Eval-Print Loop, הוא סביבת תכנות אינטראקטיבית שלוקחת קלטים בודדים מהמשתמש, מבצעת אותם, ומחזירה את התוצאה. מתכנתים משתמשים בה למשוב מיידי, ניפוי באגים, וניסוי מהיר עם מושגי קידוד ללא הצורך בקמפילציה והפעלה של תוכנית מלאה.

## איך עושים את זה:
ב-Fish, המעטפת האינטראקטיבית היא המצב הברירת מחדל כאשר אתה מתחיל אותה. כך זה נראה בפעולה:

```Fish Shell
> set color blue
> echo "השמיים הם $color"
השמיים הם כחול
```

ניתן גם להפעיל פונקציות מובנות ולשחק עם החלפות פקודות:

```Fish Shell
> function cheer
      echo "קדימה דג $argv!"
  end
> cheer מתכנתים
קדימה דג מתכנתים!
```

לא רק הגדרת פונקציות, אתה יכול גם לבצע קטעי קוד במעופף ולראות את הפלט מיד:

```Fish Shell
> math "40 / 2"
20
```

## צלילה עמוקה
המושג של REPLs חוזר אחורה לשפת התכנות Lisp בשנות ה-60. צורה זו של תכנות אינטראקטיבי הציבה את התקן לסביבות כמו `ipython` של Python ו-`irb` של Ruby. Fish ממשיך את המסורת עם דגש על ידידותיות למשתמש ושימוש אינטראקטיבי.

יתרון של Fish ביחס למעטפות אחרות כמו Bash הוא שהוא תוכנן עם דגש על אינטראקטיביות מההתחלה. הוא מספק הדגשת תחביר, הצעות אוטומטיות, והשלמות באמצעות לשונית, הופכות אותו לחזק במיוחד לשימוש באופן עבודה בסגנון REPL. טוב יותר מכך, הפקודות שלך זכורות וניתנות לחיפוש, מה שהופך בדיקות חוזרות לקלות.

אלטרנטיבות ל-REPL של Fish יכולות להיות `bash` או `zsh` כאשר הן מצורפות עם הרחבות כמו `bash-completion` או `oh-my-zsh`, אבל Fish בדרך כלל מציע חוויה עשירה יותר "מחוץ לקופסה".

## ראה גם:
- תיעוד Fish: https://fishshell.com/docs/current/index.html
- השוואה מעניינת בין Fish למעטפות אחרות: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- צלילה עמוקה יותר לתוך REPLs: https://en.wikipedia.org/wiki/Read–eval–print_loop
- תכנות אינטראקטיבי ב-Lisp, מבט היסטורי: http://www.paulgraham.com/ilisp.html
