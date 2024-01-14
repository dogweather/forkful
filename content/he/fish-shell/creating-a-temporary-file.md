---
title:    "Fish Shell: יצירת קובץ זמני"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני היא כלי חשוב בתכנות וכתיבת קוד. בעזרתו ניתן לאתחל ולאחסן נתונים זמניים ביצירת קובץ שאין צורך בו לאחר השימוש, מה שיכול לסייע ביצירת קוד יעיל ויעילות כותרתית. ניתן גם להשתמש בקבצים זמניים כדי לבצע ניסויים ואיתור באגים.

## כיצד לעשות זאת

כדי ליצור קובץ זמני ב-Fish Shell, מתבצעת הפעולה הבאה במשתנה $TMPDIR המציין את התיקייה הזמנית של מערכת ההפעלה. ניתן ליצור קובץ זמני באמצעות הפקודה touch ולאתחל את הנתונים הזמניים באמצעות פקודת echo. לדוגמה:

```Fish Shell
touch $TMPDIR/tempfile.txt
echo "This is a temporary file" >> $TMPDIR/tempfile.txt
```

אפשר גם להשתמש בתת-תיקיות בפיתרון זמני יותר נקי. לדוגמה, בכדי ליצור קובץ זמני בתקיית העבודה הנוכחית, ניתן להשתמש בקו פקודה הבא:

```Fish Shell
touch ./tempfiles/tempfile.txt
```

## מהפנים

היצירה של קובץ זמני ב-Fish Shell משתמשת בכלי בדיקה בקלות ובייעול טובים. ניתן למצוא מידע נוסף על צורת השימוש בהגדרות של שפת הסקריפט של Fish Shell ובהגדרות של פקודות פנימיות כמו set, setenv וכדומה.

## ראה גם

- [Fish Shell כדי לקבוע מדריך קבצים זמניים לעסקים עבור Bash GEO של Apple](https://blog.lansa.com/cfa-geolocations-with-fish-shell-and-bash-geo-for-apples-fish-shell)
- [התקנת גם מעבר ל-BASH!tpl עם הקריה Bashrt והשבועיים](https://medium.com/@common_python/shell/perl6-fish-shell-setup-bash-tpl-with-bashrc-and-env-5535cf5addea)