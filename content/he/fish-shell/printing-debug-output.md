---
title:                "הדפסת פלט ניתוח שגיאות בתכנות מחשבים"
html_title:           "Fish Shell: הדפסת פלט ניתוח שגיאות בתכנות מחשבים"
simple_title:         "הדפסת פלט ניתוח שגיאות בתכנות מחשבים"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

# למה

כשמפתחים קוד, נפוץ להשתמש בפקודות דיבאג כדי לבדוק ולהבין את התהליכים של התוכנית. הדפסת תוצאות debug יכולה לסייע למפתחים לזהות בעיות ולשפר את ביצועי הקוד שלהם.

## איך לעשות זאת

כדי להדפיס תוצאות debug בפקודות פיש שלך, ישנם שני אפשרויות. הראשונה היא להשתמש בפקודת `echo` עם האפשרות `-d` להדפסת תוצאות debug. לדוגמה:

```Fish Shell
echo -d "תוצאה דיבאג"
```

האפשרות השנייה היא להשתמש בפקודת `set -x` להדפסת כל השורות של הקוד שלך עם תוצאות debug. לדוגמה:

```Fish Shell
set -x
echo "תוצאה דיבאג"
```

בשני המקרים, התוצאות debug יודפסו בטרמינל שלך.

## מעמקים נמוכים

ישנם עוד כמה דברים שאפשר לעשות כדי לשפר את התוצאות debug שלכם בפיש של. לדוגמה, ניתן להשתמש בפקודה `err` כדי להדפיס תוצאות debug רק כאשר הפקודה נכשלת. ניתן גם להשתמש בפקודת `begin` ו- `end` כדי לסמן שורות ספציפיות של הקוד שבהן תוכלו להדפיס תוצאות debug.

# ראו גם

- [המדריך הרשמי של פיש על debug](https://fishshell.com/docs/current/debug.html)
- [מדריך מתקדם יותר על debug בפיש](https://medium.com/@cgarciae/the-power-of-fish-shell-debugging-114a1948e8c9)