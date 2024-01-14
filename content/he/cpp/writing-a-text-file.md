---
title:                "C++: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

כתיבת קובץ טקסט היא פעולה נפוצה וחשובה בעולם התכנות. בעזרת כתיבת קבצי טקסט, ניתן ליצור קבצים שמכילים מידע טקסטואלי ולשמור אותם על המחשב כחלק מתוכניות ואפליקציות שונות. אם אתה מתכנת או מתחיל ללמוד לתכנת בשפת C++, למד איך לכתוב קבצי טקסט על מנת להעצים את כישורי התכנות שלך.

## איך לעשות זאת

כדי לכתוב קובץ טקסט בשפת C++, אתה תצטרך להתעמק במקוד ולהשתמש בפונקציות המתאימות. הנה כמה דוגמאות של קוד C++ ליצירת קובץ טקסט ולהוספת מידע לקובץ:

```C++
#include <iostream> 
#include <fstream> 
#include <string>
using namespace std; 

int main() 
{ 
    // יצירת קובץ טקסט חדש
    ofstream file("sample.txt"); 

    // כתיבת טקסט לקובץ
    file << "זוהי דוגמא לכתיבת קובץ טקסט עם C++." << endl; 

    // סגירת הקובץ
    file.close(); 

    // קריאת קובץ טקסט קיים והוספת מידע לקובץ
    fstream file2("sample.txt",ios::app); 

    // כתיבת שורה חדשה לקובץ
    file2 << "זו עוד שורה בקובץ." << endl; 

    // סגירת הקובץ
    file2.close(); 

    // קריאת קובץ טקסט קיים והדפסת התוכן שלו
    ifstream file3("sample.txt");
    string line;
    while (getline(file3, line)) 
    {
        cout << line << endl; 
    } 

    return 0; 
} 
```

תוצאת הרצת הקוד היא קובץ טקסט חדש עם המידע המצורף בקוד הנתון.

## חפירה עמוקה

כאשר אתה כותב קובץ טקסט, יהיה חשוב לשמור על סדר בקובץ ולהשתמש בנקודה שנוכל להפעיל את הקוב