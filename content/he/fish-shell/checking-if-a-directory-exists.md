---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Fish Shell: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
לבדוק אם ספריה קיימת הוא לאמת שנתיב מסויים במערכת הקבצים מתייחס לספריה. מתכנתים עשויים לעשות זאת כדי למנוע שגיאות אם הספריה איננה קיימת ודרושה לפעולה.

## איך לעשות:
זה הקוד לבדוק אם ספריה קיימת ב-Fish Shell:

```Fish Shell
test -d "/your/path/to/directory"
```

אם הספריה קיימת, הפקודה לא תחזיר כלום, אך אם היא לא, תחזיר שגיאה:

```Fish Shell
test: Expected a combining operator like '-a' at index 2
```

## צלילה עמוקה:
בדיקת ספריות היא בעצם חלק מסביבת Unix שנוצרה בשנות ה-70. במקום לטפל בשגיאות כאשר מנת הנתיב לא קיימת, פיתחו את הפקודה "test" לבדוק מראש.

ישנן חלופות למסכנה הזו באמצעות הפקודות `if` ו`else`. לדוגמה:

```Fish Shell
if test -d "/your/path/to/directory"
    echo "Directory exists!"
else
    echo "Directory doesn't exist!"
end
```

## ראו גם:
- [מדריך Fish Shell](https://fishshell.com/docs/current/index.html)
- [פרטים נוספים על הפקודה `test`](https://fishshell.com/docs/current/commands.html#test)