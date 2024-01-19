---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Fish Shell: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פעולה שמאפשרת לנו למצוא תאריך מסוים בהתאם למרווח זמן שנקבע. מתכנתים נדרשים לבצע זאת למגוון שימושים, כולל אירגון שידורים או קידום אירועים מתוזמנים מראש.

## איך לעשות:
כדי לחשב תאריך בעתיד או בעבר ב-Fish Shell, נשתמש בפקודה 'date -v'. נוכל להוסיף או להחסיר ימים, חודשים או שנים. להלן כמה דוגמאות:
```Fish Shell
# להוסיף ימים
date -v+2d

# להוריד ימים
date -v-2d

# להוסיף שנים
date -v+1y

# להוריד שנים
date -v-1y
```
אם נריץ כל אחת מהן, נקבל את התאריך החדש זמין לשימוש.

## צלילה עמוקה
Fish Shell הוא למעשה המשך של רוב שפות ה- shell שהתפתחו החל מסוף שנות ה-70. מאז, למדו מתכנתים לזעזע במתמטיקה של הזמן כדי לתכנת את החלל הדינמי שלהם. קיימות שתי דרכים נוספות להשיג את אותו התוצאה: האחת היא לכתוב קוד מסובך באופן יחסי כדי לחשב את ההפרש בין שני תאריכים, והשניה היא להשתמש בספריות של צד שלישי אשר מאפשרות פונקציונליות זו.

## ראה גם
הקישורים להלן מתאים למידע נוסף על נושא זה, כולל קודים דוגמה ומדריכים:
* [Fish Shell בויקיפדיה](https://he.wikipedia.org/wiki/Fish_(%D8%B4%D8%A7%D9%84)
* [מדריך חישוב תאריך ב-Fish Shell](https://fishshell.com/docs/current/commands.html)
* [דיון בStackOverflow על נושא זה](https://stackoverflow.com/questions/32483837/date-calculation-in-the-fish-shell)