---
title:                "יצירת קובץ זמני"
html_title:           "Fish Shell: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא כלי פופולרי בהתכנות המאפשר לפתחים ליצור קובץ שנמחק בסופו של דבר. המטרה העיקרית של יצירת קובץ זמני היא להשתמש בו כמצביע לכתובת זמנית בזמן ריצת התוכנית, ולאחר מכן למחוק את הקובץ כדי לשחרר את השטח שנתפס על ידו.

## איך זה עובד?
שתי שורות קוד פשוטות יכולות להראות כיצד ליצור קובץ זמני ואיך למחוק אותו באמצעות Fish Shell:

```fish
# יצירת קובץ זמני
set temp_file (mktemp) 

# מחיקת קובץ זמני
rm $temp_file
```

התוצאה של הקוד הנ"ל היא קובץ זמני שנוצר בעזרת הפקודה mktemp ונמחק באמצעות הפקודה rm.

## חפירה עמוקה
יצירת קובץ זמני היא פתרון שנעשה פופולרי בעקבות הצורך להשתמש בקובץ שנמחק באופן קבוע, כגון אירועים כמו לכתוב קבצי יומן או ליצור קבצי שמירה זמנית בזמן ריצת התוכנית. ישנם גם פתרונות אחרים ליצירת קבצים זמניים כגון שימוש בפקודת touch עם כיתוביות מאופסות או בשימוש בטכנולוגיות כמו tmpfs.

## ראה גם
למידע נוסף על יצירת קבצים זמניים תוכלו לעיין במקורות הבאים:
- דוקומנטציית Fish Shell: https://fishshell.com/docs/current/commands.html#mktemp
- מדריך על יצירת קבצים זמניים עם שפת Bash: https://linuxize.com/post/bash-temporary-file/
- מאמר על יצירת קבצים זמניים בשפת C: https://www.ibm.com/docs/en/openshift/3.11.171/creating-temporary-files-in-c