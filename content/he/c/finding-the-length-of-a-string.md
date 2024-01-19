---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת ב-C הוא פעולה שבה אנחנו סופרים את מספר התווים שבמחרוזת, לא כולל התו המסיימת 'NULL'. מתכנתים עשויים לשם דבר זה לוודא כי המחרוזת מחזיקה בתוך זיכרון הדינמי או לבחון את התאימות של מחרוזת מסוימת לתנאים ספציפיים.

## איך לעשות:
קוד המדגים את מציאת האורך של מחרוזת:
```C
#include<stdio.h>
#include<string.h>

int main() 
{
   char str[100];
   int length;

   printf("הכנס מחרוזת: ");
   gets(str);

   length = strlen(str);

   printf("אורך המחרוזת הוא: %d", length);
   
   return 0;
}
```
כאשר המשתמש מזין את המחרוזת "בדיקה", הפלט הוא "אורך המחרוזת הוא: 6".

## הצצה מעמיקה:
פונקציית ה-strlen שבמערכת היא חלק מהספרייה התקנית של C ומשמשת למדידת אורך מחרוזת. ראשית, הפונקציה התפתחה כחלק מהשפה ב-1972. 

השיטה האלטרנאטיבית מצריכה לולאה שמסתכלת על כל תו במחרוזת עד שהיא מגיעה ל-NULL, מספרת כל התווים בדרך.

אינפורמציה בנוסף, זמן הביצוע של הפונקציה הוא O(n), כאשר n הוא אורך המחרוזת. זה מאוד ברור כאשר מתבוננים בנוסחה שמחשבת את אורך המחרוזת- זו חייבת לעבור על כל התווים שבה כדי לספור אותם.

## ראה גם:
1. [strlen function (Wikipedia)](https://en.wikipedia.org/wiki/C_string_handling#strlen)
2. [String handling functions in C (GeeksforGeeks)](https://www.geeksforgeeks.org/string-handling-functions-in-c-with-examples/)
3. [C string.h Library (Programiz)](https://www.programiz.com/c-programming/library-function/string.h/)