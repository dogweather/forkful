---
title:                "C++: יצירת קובץ זמני"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

על כל חובב תכנות רציני לפעמים לצורך יצירת קובץ זמני במהלך כתיבת קוד. הקובץ זמני מיוצר ומשמש לכמה מטרות שונות, כגון כתיבת נתונים זמניים, בדיקת קוד או יצירת קבצי לוגים זמניים. בפרסום הזה נלמד כיצד ליצור קובץ זמני בשפת סי++.

## כיצד אנחנו יוצרים קובץ זמני בסי++

בשפת סי++ ישנם מספר דרכים ליצור קובץ זמני. ניתן להשתמש בפונקציות מוגדרות מראש מהספרייה הסטנדרטית או ליצור פתרונות מותאמים אישית. נדגים כאן שתי דרכים ליצור קובץ זמני בשפת סי++.

פתרון ראשון הוא באמצעות הפונקציה `tmpfile()` מהספרייה הסטנדרטית. התוכנית הבאה מדגימה יצירת קובץ זמני וכתיבה של מחרוזת לתוכו.

```C++ 
#include <cstdio> 
  
int main() 
{ 
    // יצירת קובץ זמני באמצעות הפונקציה tmpfile() 
    FILE* temp = tmpfile(); 
      
   
    if (temp == NULL) 
        printf("Failed to create temporary file."); 
    else
    { 
        printf("Temporary file created successfully!\n"); 
  
        // כתיבת מחרוזת לקובץ 
        fprintf(temp, "Hello from temporary file!"); 
          
        // סגירת הקובץ 
        fclose(temp); 
    } 
    return 0; 
} 
```

פתרון שני הוא באמצעות פונקציית `mkstemp()` מהספרייה הסטנדרטית. התוכנית הבאה מדגימה יצירת קובץ זמני עם שם ייחודי וכתיבה של מחרוזת לתוכו.

```C++ 
#include <cstdio> 
  
int main() 
{ 
   // מטרה ליצור את הקובץ "/tmp/temp-XXXXXX" 
   char temp[] = "/tmp/temp-XXXXXX"; 
  
   
   // יצירת קובץ זמני עם שם ייחודי באמצעות הפונקציה mkstemp() 
   int fd = mkstemp(temp); 
  
   
   if (fd == -1