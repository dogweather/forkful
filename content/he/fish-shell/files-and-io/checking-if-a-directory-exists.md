---
title:                "בדיקה אם ספרייה קיימת"
date:                  2024-02-03T19:07:44.375977-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה האם ספרייה קיימת ב-Fish Shell מאפשרת לסקריפטים לקבל החלטות בהתבסס על הנוכחות או היעדר של מבני תיקיות, דבר המאפשר ביצוע משימות כמו פעולות קבצים מותנות, רישום לוגים או הקמת סביבה. טכניקה זו קריטית לכתיבת סקריפטים חזקים המתקשרים עם מערכת הקבצים בצורה צפויה.

## איך לעשות זאת:
Fish Shell משתמש בפקודת `test` כדי לבדוק סוגי קבצים ותכונותיהם, כולל אם היעד הוא ספרייה. הנה תבנית בסיסית לבדיקה אם ספרייה קיימת:

```fish
if test -d /path/to/dir
    echo "הספרייה קיימת"
else
    echo "הספרייה אינה קיימת"
end
```
פלט לדוגמה:
```
הספרייה קיימת
```

לצורך פעולות קבצים וספריות יעילות יותר, ייתכן שאחד יעבור לכלים חיצוניים כמו `fd`, עם זאת זה בדרך כלל נמצא בשימוש לחיפוש קבצים ותיקיות ולא רק לבדיקה של קיום. בכל אופן, שילוב שלו עם סקריפטים של Fish יכול לתת תוצאות שימושיות:

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "הספרייה קיימת"
else
    echo "הספרייה אינה קיימת"
end
```

דוגמת ה-`fd` הזו מחפשת את הספרייה בעומק נתון, ו-`grep` בודק להתאמה, מה שהופך אותה לגמישה לבדיקות מורכבות. עם זאת, למטרת הבדיקה הישירה של קיום, הסתמכות על ה-`test` המובנה של Fish היא גם יעילה וגם פשוטה.