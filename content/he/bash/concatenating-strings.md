---
title:                "Bash: כינון מחרוזות"
simple_title:         "כינון מחרוזות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

בלוג על תכנות Bash עבור קוראים עברית

## למה:
למה יש לנו צורך לצרף מחרוזות בתוכניות שלנו? צירוף של מחרוזות הוא כלי חשוב בתכנות שיעזור לנו ליצור מחרוזות ארוכות ומורכבות. זה יכול לשמש ליצירת הודעות, הצגת מידע ועוד.

## כיצד לבצע:
ישנם מספר דרכים לצרף מחרוזות בתוכניות Bash. נוכל להשתמש בפקודת echo עם פרמטר -e כדי להדפיס מחרוזת עם תווים מיוחדים, כגון \n לשורת חדשה. נוכל גם להשתמש בפקודת printf ובפרמטר %s כדי להדפיס מחרוזת תוך שימוש במשתנים או בתווים קבועים. ניתן גם להשתמש בפקודת cat ובפרמטר -e כדי להדפיס מחרוזת תוך שימוש בקובץ טקסט למצבל.

```Bash
# Exapmles of string concatenation using echo
echo -e "Hello, my name is $NAME"

# Using printf with variables
printf "My favorite color is %s and my favorite food is %s" $COLOR $FOOD

# Concatenating string from a text file using cat
cat -e $FILE
```

הפלט הצפוי הוא:
```
Hello, my name is Sarah
My favorite color is blue and my favorite food is pizza
Hello
My name is
Sarah
```

## מעמקים נמוכים:
לצרף מחרוזות בתוכניות Bash ניתן להשתמש גם באופרטור "+". האופרטור הזה יפעול בדיוק כמו אופרטור החיבור במתמטיקה ויצרף ישויות ביחד. בנוסף, ניתן להשתמש גם בפקודת substring כדי לקבל חלק מתחילת המחרוזת או ממיקום מסוים בה.

## ראה גם:
- [שימוש במערך של מחרוזות בבאש](https://linuxize.com/post/bash-concatenate-strings/)
- [פייפר עם הוספת מחרוזות](https://stackoverflow.com/questions/4181703/how-to-concatenate-string-variables-in-bash)
- [התמודדות עם מחרוזות מורכבות בבאש](https://www.baeldung.com/linux/concatenate-strings-bash)