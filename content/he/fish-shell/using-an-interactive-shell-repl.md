---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
aliases:
- he/fish-shell/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:14:41.658355-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/using-an-interactive-shell-repl.md"
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
