---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
aliases:
- he/bash/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:11:55.691770-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
REPL זה ראשי תיבות של Read-Eval-Print Loop, סביבת תכנות אינטראקטיבית ופשוטה. מתכנתים משתמשים בה כדי לכתוב ולבדוק קוד במהירות, לנסות תחבירים שונים, וללמוד רעיונות תכנותיים ללא הצורך ביצירת והרצת יישומים שלמים.

## איך לעשות:
ב-Bash, הטרמינל שלך הוא למעשה REPL. אתה מקליד פקודה; היא קוראת אותה, מעריכה אותה, מדפיסה את התוצאה, וחוזרת להמתין לפקודה הבאה שלך. הנה דוגמה לשימוש ב-Bash כ-REPL:

```Bash
$ echo "שלום, עולם!"
שלום, עולם!
$ x=$((6 * 7))
$ echo $x
42
```

הקלט שלך מתחיל אחרי הסימן `$ `, והפלט מודפס בשורה הבאה. פשוט, נכון?

## צלילה עמוקה
Bash, שהמשמעות שלו היא Bourne Again SHell, הוא ה-shell המוגדר כברירת מחדל על מערכות מבוססות Unix רבות. זו שדרוג ל-shell המקורי של Bourne, שנבנה בסוף שנות ה-70. למרות ש-Bash הוא כלי סקריפטינג עוצמתי, המצב האינטראקטיבי שלו מאפשר לך לבצע פקודות שורה אחר שורה.

כאשר מחפשים חלופות, יש לך את REPL של Python (פשוט כתוב `python` בטרמינל שלך), Node.js (עם `node`), ו-IPython, שלל Python אינטראקטיבי משופר. כל שפה נוטה להציע את היישום ה-REPL שלה.

מתחת לכל אלה, ה-REPLs הם לולאות שמנתחות את הקלט שלך (פקודות או קוד), מריצות אותו, ומחזירות את התוצאה ל-stdout (המסך שלך), לעיתים קרובות תוך שימוש ישיר במתורגמן של השפה. זו מיידיות של משוב היא מעולה ללמידה ולפרוטוטיפינג.

## ראה גם
- [תיעוד GNU Bash הרשמי](https://gnu.org/software/bash/manual/bash.html)
- [למד Shell באופן אינטראקטיבי](https://www.learnshell.org/)
- [אתר הרשמי של IPython](https://ipython.org/)
- [REPL.it](https://replit.com/): REPL אינטרנטי רב-שפתי (לא רק Bash!)
