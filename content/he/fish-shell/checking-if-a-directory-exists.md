---
title:                "Fish Shell: בדיקת קיום תיקייה"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

כשמבצעים תכנות של הַדריווכאת, חשוב לוודא שהַאיִדריי קיים והמידע המבוקש ימצא במקומו הנכון.

## איך לבדוק אם ספרייה קיימת בְ־Fish Shell

כדי לבדוק אם ספרייה קיימת בְ־Fish Shell, ניתן להשתמש בפקודה הבאה:

```Fish Shell

if test -d path/to/directory

    echo "The directory exists."

else

    echo "The directory does not exist."

end

```

תיאור הקוד: בעזרת הפקודה test והפרמטר -d, אנחנו בודקים אם הנתיב שמופנה קיים ואם כן, נדפיס הודעה מתאימה. אם הנתיב לא קיים, נדפיס הודעה אחרת.

הנה דוגמא נוספת תוך שימוש ב־if-else ב־Fish Shell:

```Fish Shell

if [ -d "path/to/directory" ]; then

    echo "The directory exists."

else

    echo "The directory does not exist."

fi

```

בדוגמא זו, אנו משתמשים בסוג יותר מתקדם של if-else כדי לבדוק אם הנתיב קיים או לא.

## לחקור עמוק יותר

כעת שיעוררו הכיוונים בנוגע לבדיקת קיום ספרייה, נוכל להתעמק יותר ולדעת שפקודת test נועדה לבדוק גם קיום קבצים, לא רק ספריות. ניתן לשנות את הפרמטר המשתנה -d ל-p כדי לבדוק קיום קבצים במקום ספריות. כמו כן, ניתן להשתמש בפקודות אחרות כגון stat, כדי לקבל מידע מפורט יותר על קבצים או ספריות ספציפיות.

## ראה גם

- [פקודת test ב־Fish Shell](https://fishshell.com/docs/current/cmds/test.html)
- [עיצוב תכניות ב־Fish Shell](https://fishshell.com/docs/current/design.html)
- [פקודת stat ב־Linux](https://linux.die.net/man/1/stat)