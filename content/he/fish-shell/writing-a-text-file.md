---
title:                "כתיבה לקובץ טקסט"
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא התהליך שבו אתם יוצרים קובץ חדש או מעדכנים קובץ קיים עם טקסט. תוכניתנים עושים זאת לשמירת נתונים, תצורות ולוגים.

## איך עושים זאת:
כדי לכתוב לקובץ ב-Fish, השתמשו בפקודות כמו `echo` ו-`tee`.

```Fish Shell
# כתיבה לקובץ חדש
echo "שלום, עולם!" > hello.txt

# הוספה לקובץ קיים
echo "זהו קובץ קיים!" >> existing_file.txt

# כתיבה לקובץ באמצעות tee
echo "שורה חדשה" | tee -a file_with_tee.txt
```

הוצאה לדוגמה:

```
# אין הוצאה בקונסול אלא אם נעשה שימוש ב-flag -a בפקודת tee
```

## צלילה לעומק:
בעבר, כלים כמו `ed` או `sed` היו שימושיים לעריכת קבצים במערכות Unix. Fish מציעה גישה אינטואיטיבית יותר עם פקודות שחוברות ישירות ל-shell. עלתרנטיבות נוספות כוללות כתיבה באמצעות תוכניות עריכה אוטומטיות כמו `awk` או השימוש בשפות תכנות כמו Python או Perl. כשכותבים קובץ, המערכת הבסיסית מייצרת inode שמקשר את הקובץ למחיצה הפיזית שלו.

## ראו גם:
- [Fish Documentation on Redirecting Output](https://fishshell.com/docs/current/index.html#redirects)
- [GNU Coreutils: tee](https://www.gnu.org/software/coreutils/manual/html_node/tee-invocation.html)
- [Unix Shell Scripting Tutorial](https://www.shellscript.sh/)