---
title:    "C++: חיפוש והחלפת טקסט"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

מדוע: הטכניקה של החיפוש וההחלפה של טקסט היא יכולה להיות מאוד שימושית כאשר מתעסקים עם תכניות גדולות ומורכבות בסי++.

כיצד: קוד פשוט יכול לעזור לנו לבצע תהליך זה בצורה מאורגנת ומהירה. להלן דוגמאות של כיצד להשתמש בחיפוש והחלפה של טקסט בשפת סי++:

 ```C++
 #include <iostream>
 #include <string>
 using namespace std;

int main() {

  // טקסט לחיפוש והחלפה
  string text = "היי עולם, היי סי++";

  // חיפוש המחרוזת "היי"
  int index = text.find("היי");
  // החלפת החרוזות
  text.replace(index, 3, "אהלן");

  // הדפסת התוצאה
  cout << text << endl;

  return 0;
}
 ```

פלט: אהלן עולם, אהלן סי++

נדון עומק: חיפוש והחלפת טקסט הם טכניקות מאוד נפוצות ושימושיות בסי++, מאחר והן מאפשרות לנו לשנות את המידע על המחשב שלנו בצורה יעילה ומהירה. אולם, כדי להשתמש בטכניקות האלו בצורה יעילה, חשוב להכיר היטב את תכניות המחשב ואת הפעולות האפשריות עם מחרוזות.

ניתן להשתמש בפונקציות כמו find, replace ו-substr על מנת למצוא ולהחליף מחרוזות במידת הצורך. מומלץ לבדוק תיעוד ודוגמאות לפני השימוש בהן.

ראו גם: [תיעוד של פונקציות מחרוזות בסי++](https://www.cplusplus.com/reference/string/string/), [דוגמאות לשימוש בחיפוש והחלפת מחרוזות בסי++](https://www.geeksforgeeks.org/string-find-in-cpp/), [הבדלים בין ספריות string ו-cstring בסי++](https://stackoverflow.com/questions/13472164/c-performance-string-vs-cstring/13472502#13472502)