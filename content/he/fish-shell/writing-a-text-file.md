---
title:                "Fish Shell: Esrim lechateret al programmazia lechachot sheima"
simple_title:         "Esrim lechateret al programmazia lechachot sheima"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה:

כתיבת קובץ טקסט נחשב לכדאי למגוון סיבות, כולל יצירת תסדיר רשימת משימות, יצירת תיאורים לקבצים או פשוט להעביר מידע בצורה מהירה ופשוטה.

## איך לעשות:

קובץ טקסט ניתן ליצור על ידי פקודות של Fish Shell, כמו "echo" לדוגמה. להלן דוגמא פשוטה לכתיבת כמה שורות לקובץ טקסט עם "echo":

```
Fish Shell
echo "שלום לעולם!" >> הרצף
echo "זוהי יותר נוח מאשר לכתוב נתונים ידנית לקובץ." >> הרצף
echo "עוד דוגמאות לשימושים: יצירת רשימת קבצים, נתוני הגדרות וכו'." >> הרצף
echo "אפשר גם להוסיף נתונים מקובץ אחר באמצעות >>, כעת אתה יכול לקרוא אותם כאן." >> הרצף
```

### פלט:
```
שלום לעולם!
זוהי יותר נוח מאשר לכתוב נתונים ידנית לקובץ.
עוד דוגמאות לשימושים: יצירת רשימת קבצים, נתוני הגדרות וכו'.
אפשר גם להוסיף נתונים מקובץ אחר באמצעות >>, כעת אתה יכול לקרוא אותם כאן.
```

## העומק:

כדי לכתוב קובץ טקסט עם פיש של, ישנן אפשרויות רבות שאפשר להשתמש בהן, כולל השתמש בפקודת "cat" ולערוך קובץ קיים או להשתמש בתוכנות נוספות כמו פייתון ו-Perl עבור יצירת קבצים מתקדמים.

## ראה גם:

- [קורס מבוא לפיש של](https://fishshell.com/docs/current/tutorial.html) 
- [מדריך למתחילים על כתיבת טקסט בפיש של](https://fishshell.com/docs/current/tutorial.html#4---writing-text-with-fish)
- [מדריך לכתיבת