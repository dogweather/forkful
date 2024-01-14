---
title:                "Fish Shell: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#למה

חישוב תאריך בעתיד או בעבר יכול להיות שימושי עבור מגוון של מטרות, כגון ייעוד וילד עסקים או ימי הבחירות החלים. במאמר זה נלמד איך לחשב תאריך בעזרת השפת פיש שליל, שיאפשר לנו לבצע זאת באופן מהיר ויעיל.

#איך לעשות 

כדי לבצע חישוב תאריך בעתיד או בעבר באמצעות פיש שליל, ניתן להשתמש בתפקיד המובנה date. למטה תוכלו למצוא שלושה דוגמאות של קוד עם פלט קשור, המדגימות את השימוש בפקודה זו.

```Fish Shell
date 1 day from now
```

בפלט לעיל נוכל לראות את התאריך של יום אחרי היום הנוכחי.

```Fish Shell
date 1 week ago
```

בדוגמה זו, ניתן לראות את התאריך של שבוע לפני היום הנוכחי.

```Fish Shell
echo "The next election day is on (date -v +5y)"
```

בקוד זה, ניתן לראות דוגמה של שימוש בתפקיד המובנה של date כדי להדפיס תאריך אקראי בעתיד, במקרה זה 5 שנים מעכשיו. 

#העמקה מעמיקה

כמו שראינו בדוגמאות הנ"ל, ניתן להשתמש בתפקיד המובנה ובפקודה date כדי ליצור תאריכים בעתיד או בעבר. ניתן גם להשתמש בפרמטרים נוספים כמו 'years', 'months', 'weeks' וכדומה כדי לייצר תאריכים בהתאם לצורך. כמו כן, ניתן להשתמש בפקודה date עם תאריך מדוייק כדי לבצע חישובים מורכבים יותר.

#ראה גם

- [מדריך לשימוש בפקודת date בפיש שליל](https://fishshell.com/docs/current/cmds/date.html)
- [למד את השפה: פיש שליל למתחילים](https://fishshell.com/docs/current/tutorial.html)
- [פונקציות מובנות בפיש שליל](https://fishshell