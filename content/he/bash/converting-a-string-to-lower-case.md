---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Bash: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מדוע

ישנם כמה סיבות למה לחלץ את הכתביות באמצעות ערכת הכתביות

## איך לעשות

החילוץ של כתביות מרובת האופציות הזמינות שבשפת הבאש ניתן לבצע פעולת חילוץ בעזרת פקודת "tr" כדי לשנות את התווית בכתביות לתווית נמוכה באמצעות הפקודה "to lower" כדי לקלט כל Input אחד עם הפקודה

```Bash
tr '[:upper:]' '[:lower:]' < input.txt
```

כתוב בפקודת `tr` הכתביות תהפכנה לתווית נמוכה.

וכדי לייצר Output חלק רשימה של שמות הקבצים עם השמות הנמוכים שלהם, ניתן להשתמש בפקודה "ls" כדי להציג את הקבצים עם הקבצים הנמוכים.



```Bash
tr '[:upper:]' '[:lower:]' < input.txt | ls 
```

כל Output יהיה כעת רשימת כתוביות עם כל הקבצים הנמוכים בצדדים שלהם.

##עומק עמוק

כתביות נמוכים היא יכולת עצלים מאוד פתיחת הקבצים הנמוכים שלך, פעם אחת בדפדפן באמצעות הפקודה "tr" כדי ל"הפעיל" אפלודציה שלך גם במקרה שבו אינך בא לבדוק את הקבצים הנמוכים שלך.

##ראה גם

- [רשימת סיבות לכתוב *טיפלונות* של הפקודט של יאלדור] (https://www.indeed.com/career-advice/finding-a-job/benefits-of-remote-work) 
- [מדריך לגיבוי ושחזור קובטי] (https://zapier.com/blog/tie-your-applications-together/) 
- [מדריך ליצירת קובץ Mailtras עם באשככת צ'ילה] (https://www.howtoonditor.com/guideafaq/using-mailtrais-with-bash)