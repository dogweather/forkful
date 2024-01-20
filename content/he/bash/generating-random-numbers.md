---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
גנרציה של מספרים אקראיים היא יצירה של מספר הבא באופן חסר תיאום, ללא הגיון ניכר. מתכנתים משתמשים בזה כדי להגביר את הביטחון, לבדיקות אוטומטיות, לפרטי משחק ועוד.

## איך ל־:
הנה דוגמה איך ליצור מספר אקראי בשפת Bash:

```Bash
#!/bin/bash
echo $(( RANDOM % 10 ))
```

כאשר תריצו את הסקריפט הזה, הוא ידפיס מספר אקראי בין 0 ל-9.

## עומק
הגנרציה של מספרים אקראיים במחשבים החלה במחקר באנגליה בשנות ה-40, במטרה לפתור בעיות אקראיות בשימוש במחשבים. יחד עם Bash, קיימות אפשרויות רבות אחרות ליצירת מספרים אקראיים, כולל `openssl`, PHP, Python ועוד. Bash משתמש במשתנה מיוחד, `$RANDOM`, המחזיר מספר אקראי בין 0 ו-32,767. השימוש ב-% מאפשר להגביל את הטווח של המספרים האקראיים.

## ר' גם:
- [גנרציה של מספרים אקראיים ב-Python](https://realpython.com/python-random/)
- [ספריית openssl](https://www.openssl.org/docs/manmaster/man3/RAND_bytes.html)