---
title:                "Fish Shell: מחיקת תווים שעונים לתבנית."
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

כיצד: דוגמאות קוד ופלט מוצגים בתוך קוד הגדרה של Fish Shell

## מעמיקים

מידע מעמיק על מחיקת תווים התואמים לתבנית

See Also:

- כיצד למחוק קבצים בעזרת Fish Shell: [https://www.howtogeek.com/259638/how-to-delete-files-in-linux-using-the-fish-shell/](https://www.howtogeek.com/259638/how-to-delete-files-in-linux-using-the-fish-shell/)
- Fish Shell ב-Wikipedia: [https://en.wikipedia.org/wiki/Fish_(Unix_shell)](https://en.wikipedia.org/wiki/Fish_(Unix_shell))

מחקת תווים התואמים לתבנית היא כלי חזק שניתן להשתמש בו בפקודות של ה־Fish Shell. וודא שיש לך את התווים המתאימים ותואמים למסמך או הקבצים שברצונך למחוק ואז השתמש בפקודה הבאה: 

```
fish> rm *אות
```

הפקודה הזו תמחק את כל התווים שתואמים לתבנית שנמצאת לאחר האות שצוינה. לדוגמה, אם נרצה למחוק את כל התיקיות שמתחילות באות "a", נכתוב:

```
fish> rm -rf *א
```

זה ימחק את כל התיקיות שמתחילות באות "a" יחד עם כל התת־תיקיות והקבצים שנמצאים בתוךם. מיתור התווים הנשארים בתוך הפקודה חשוב להציג ולהשתמש בפקודה:

```
fish> rm -i *א
```

כדי לוודא שאתה מוחק את הקבצים הנכונים. אם אתה לא מעוניין באישור למחיקת כל קובץ וכל תיקייה אתה יכול ליצור כברירת מחדל שמוודא את השימוש בסימון:

```
$ set rm_confirm
```

כמו שאתה יכול לראות, מחקת תווים תואמים לתבנית היא טכניקה יעילה כדי לנקות ולסדר את התיקיות והקבצים החוזרים ממאגרי קבצים גדולים. כל מה שנצטרך לעשות הוא להיות כמה שיותר דוק בהתאם להנחיות, ולהשתמ