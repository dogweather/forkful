---
title:    "PHP: המרת תאריך למחרוזת"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

מדוע: מתי יש צורך להמיר תאריך למחרוזת? גוף של 1-2 משפטים עונה על השאלה המדוע 

כיצד לפעול: דוגמאות קידוד ופלט מצורפים בתוך בלוקי קוד של "``` PHP ... ```". 

```PHP
$date = "05/15/2021";
echo date("F j, Y", strtotime($date)); // יוצא: מאי 15, 2021
echo date("d/m/Y", strtotime($date)); // יוצא: 15/05/2021 
```

קישור עמוק: נתונים נוספים על המרת תאריך למחרוזת וכל האפשרויות השונות שקיימות בפעולה זו. 

מסוף מסמך: ראה גם 

- המדריך המלא: https://www.php.net/manual/en/function.date.php
- סרטון הדרכה: https://www.youtube.com/watch?v=Uoj08amOZYY
- דוגמאות נוספות: https://www.php.net/manual/en/datetime.formats.date.php

תודה על קריאת המאמר ובהצלחה בתהליך העבודה שלכם עם פיתוח אתרים ב-PHP!