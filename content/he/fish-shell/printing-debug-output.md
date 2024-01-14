---
title:                "Fish Shell: הדפסת פלט מנקה שגיאות"
simple_title:         "הדפסת פלט מנקה שגיאות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה

ניתן להשים פלט דיבאג כדי לעזור למפתחים לנתח ולתקן בעיות בקוד שלהם. 

## איך לעשות זאת

```Fish Shell
echo "פלט זה הינו דיבאג"
```

כדי להציג תיבת הודעה בדיבאג, ניתן להשתמש בפקודה `echo` ולהעביר לה את הטקסט שאתה מעוניין להדפיס. אם אתה מעדיף להשתמש במשתנים או בתנאי גם ניתן לעשות זאת בעזרת הפקודה `printf`.

```Fish Shell
set variable "התנהגות זו משתנת"
printf "המשתנה שלי הוא %s" $variable
```

כאשר מריצים את הקוד, ניתן לראות את הפלט המלא בטרמינל.

## צלילה עמוקה

מוטיבים נוספים להדפסת פלט דיבאג כוללים שימוש במודגש להשלמת קוד בזמן פיתוח וניתוח קוד, או בתור כלי לבחינת תהליכי שימוש וניתוח.

## ראה עוד

- [דוגמאות נוספות בדוגמת הדפסת דיבאג בשפת Fish Shell](https://fishshell.com/docs/current/tutorial.html#print-debugging)
- [מדריך על אמנות תחנך של פלט דיבאג שאמור לעוזר לעבודה שלך](https://docstore.mik.ua/orelly/webprog/DiveIntoR01_[...])