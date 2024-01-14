---
title:                "Fish Shell: יצירת קובץ זמני"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# למה:

יצירת קובץ זמני היא בדרך כלל צעד חשוב בתהליך התכנות ב-Fish Shell. הקובץ הזמני מאפשר לנו לשמור ולעבד מידע בעת ריצת התכנית ולאחר מכן למחוקו כדי לשמור על ניקיון במערכת שלנו.

## איך לעשות זאת:

בקוד הבא תוכלו לראות דוגמה של איך ליצור קובץ זמני ב-Fish Shell ולהדפיס את התוכן שלו:

```fish
# יצירת קובץ זמני בשם "tempfile.txt"
set tmp (mktemp)

# כתיבה של טקסט כלשהו לתוך הקובץ זמני
echo "זוהי הודעה זמנית" > $tmp

# קריאה של תוכן הקובץ והדפסתו
cat $tmp

# מחיקת הקובץ הזמני לאחר השימוש
rm $tmp
```

פלט הפקודות הללו יהיה:

```
זוהי הודעה זמנית
```

בעזרת קוד זה, אתם יכולים ליצור קבצים זמניים כמו קבצי טקסט, תמונות או קבצי קוד ולעבוד עליהם כל עוד התוכנית מופעלת.

## חפירה עמוקה:

יצירת קובץ זמני ב-Fish Shell מבצעת מספר פעולות ומשתנה בהתאם למערכת הפעולות של מערכת ההפעלה שלכם. אם אתם מעוניינים לחפש פרטים נוספים על אופן ביצוע פעולות אלה, מומלץ לעיין בתיעוד של Fish Shell ולהתנסות בקוד כדי לראות כיצד הענקת ההרשאות והגנה על המידע ניתנת לביצוע.

## ראו גם:

- [תיעוד Fish Shell](https://fishshell.com/docs/current/)
- [מדריך מתחילים ל-Fish Shell](https://dev.to/alekswrites/building-a-fish-shell-for-beginners-4kka)