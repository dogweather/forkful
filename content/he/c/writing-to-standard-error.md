---
title:                "C: כתיבה לשגיאת סטנדרט"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

כתיבה לפלט שגיאה תקני היא חלק חשוב מכתיבת קוד בשפת סי. העברת שגיאות לפלט יכולה לעזור למפתחים לזהות בעיות בקוד ולתקן אותם בקלות.

## איך לעשות זאת

על מנת לכתוב לפלט שגיאה בשפת סי, יש להשתמש בפונקציית fprintf ולציין את הגוף (stdout) שאליו תתבצע הכתיבה. ניתן להשתמש בתבנית הבאה:

```C
fprintf(stdout, "הודעת שגיאה כאן");
```

אם רוצים לכתוב לפלט שגיאה עם פרטי תקלה נוספים, ניתן להשתמש בפונקציית perror ולכתוב את השגיאה כפרמטר שלה:

```C
perror("פרטי השגיאה כאן");
```

הפונקציות fprintf ו-perror מאפשרות להדפיס לפלט שגיאה בכל נקודה בקוד, לא משנה באיזה מצב התוכנית נמצאת.

## העומק

הוספת הכתיבה לפלט שגיאה שלך מוסיפה מרכיב חשוב למתמטיקה של תיקון שגיאות. חשוב לזכור שעל מנת שכתיבת השגיאה תהיה שימושית, עלינו לוודא שאנחנו משתמשים בה רק במקרים של באגים אמיתיים ולא בכל פעם שאנחנו עומדים בתנאי אחר.

## ראה גם

- [כתיבה לפלט שגיאה בשפת סי](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [מדריך לכתיבת שגיאות כתובות במקצת (StackTrace)](https://www.geeksforgeeks.org/how-to-print-error-message-in-c/)
- [דוגמאות נוספות לכתיבת שגיאות בשפת סי](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)