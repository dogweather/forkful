---
title:                "Fish Shell: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני היא כלי מאוד שימושי בתכנות בשפת פיש של סביבת העבודה. הקובץ הזמני מאפשר לנו לאתחל ולנקות תוכן זמני במחשב שלנו בצורה נוחה ומהירה.

## איך לעשות

תחילה, ניצור קובץ זמני באמצעות הפקודה "mktemp", כך:

```Fish Shell
set temp_file (mktemp)
```

לאחר מכן, נוסיף תוכן לקובץ זמני שנוצר כך:

```Fish Shell
echo "זוהי ההודעה הראשונה בקובץ" > $temp_file
```

ניתן למצוא את התוכן שנוצר על ידי להדפיס את תוכן הקובץ:

```Fish Shell
cat $temp_file
```

כעת, כשכבר עבדנו עם הקובץ זמני, נוכל למחוק אותו באמצעות הפקודה "rm" כך:

```Fish Shell
rm $temp_file
```

## מודחק מעמיק

יצירת קובץ זמני מקלה עלינו את התכנות כאשר אנחנו צריכים לעבוד עם תוכן זמני. חשוב לזכור שהקובץ זמני יימחק באופן אוטומטי כאשר סיימנו לעבוד איתו, כך שלא יינתן לו להישאר במחשב ולצרוך מקום על חבילתנו. נוכל להשתמש בקובץ זמני כדי לכתוב לוגים או ליצור קבצי תיקונים זמניים.

## ראה גם

- ההוראות המלאות לקריאה על פקודת "mktemp" ושימושיה: https://fishshell.com/docs/current/cmds/mktemp.html
- הספריה הנפוצה לניהול קבצים זמניים בפייתון: https://tempfile.readthedocs.io/en/latest/ 
- כיצד להשתמש בפקודת "cat" כדי לקרוא ולהדפיס תוכן של קובץ: https://www.computerhope.com/unix/ucat.htm