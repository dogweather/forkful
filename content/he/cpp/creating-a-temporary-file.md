---
title:                "C++: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה
יצירת קובץ זמני יכול להיות חסכוני ושימושי ביותר כאשר את/ה מפתח/ת תוכניות ב- C++ ואנכה מבצע/ת מניפולציות על קבצים זמניים זה יעזור לשמור על סדר ובמקרה הצפון, לאבד משאבים אחרים.

## כיצד לעשות זאת
כדי ליצור קובץ זמני ב-C++, הכי פשוט היא להשתמש בפונקציית tmpnam שמקבלת בתור פרמטר מצב קבוע. לדוגמא:

```C++
#include <stdio.h>
#include <stdlib.h>
int main () {
   FILE * fp;
   char tmpname [L_tmpnam];
   char * filename;
   strcpy (tmpname, tmpnam(NULL));
   filename = tmpnam (tmpname);
   printf ("File name: %s\n", filename);
   fp = fopen (filename, "w");
   fclose(fp);
   return(0);
}
```

תוצאת התוכנית תהיה כזאת:

`File name: D:\tmp\fileM0FSsap`

## צלילה עמוקה
קבצים זמניים הם קבצים שייצרו במערכת הפעלה. הם משמשים כמקום זמני לשמירת מידע והם נמחקים כשהתוכנית מצאת שימוש בהם שנית. על המפתח יהיה עליו למחוק את הקובץ הזמני כמו שהוא נעשה עם כל קובץ רגיל אחר.

## ראו גם.
- [קבצים זמניים בפייתון](https://www.geeksforgeeks.org/temporary-files-python/)
- [מסמכנציאה: יצירה של קבצים זמניים](https://www.microsoft.com/he-il/microsoftteams/blog/creatings-and-using-tmps/)

זהו פוסט פשוט וידידותי למתחילים שמסביר לכם כיצד ליצור קובץ זמני ב-C++. כעת, את/ה יכול/ה להשתמש בטכניקה זו בתוכניות שלך כדי לטפל בקבצים במהירות ויעילות. תהנו!