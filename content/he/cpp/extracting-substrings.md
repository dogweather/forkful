---
title:                "C++: חילוץ תת־מחרוזות"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

כאשר מכינים קוד לטיפול במחרוזות בשפת סי-פלוספלוס (C++), בדרך כלל נדרש לחלץ מחרוזת מסוימת או חלק ממנה. דוגמאות למקרים שבהם ייתכן להיות צורך בחילוץ חלקי מחרוזות הם כאשר רוצים לקרוא מספר טלפון מתוך מחרוזת מבולטת או כאשר רוצים להבין את הכתובת האימייל בעזרת חלוקה לשם משתמש ודומיין. לכן, ייתכן שתרצו ללמוד כיצד לחלץ מחרוזות כדי לטפל במקרים כאלה וכדי לשפר את כישורי התכנות שלכם.

## איך לעשות זאת

לחילוץ תת-מחרוזות בשפת סי-פלוספלוס קיימות שתי אפשרויות עיקריות - שימוש במתודות מובנות או כתיב פונקציה משלכם. הנה דוגמאות קוד לשתי האפשרויות והפלט המתקבל:

```C++
// שימוש במתודות מובנות:
#include <iostream>
#include <string>

using namespace std;

int main(){
  string text = "זהו מחרוזת לדוגמה";

  // חילוץ תת-מחרוזת מהתחלה עד מיקום סופי נתון:
  cout << text.substr(0, 4) << endl; // הדפס "זהו"

  // חילוץ החל ממיקום נתון ועד סוף המחרוזת:
  cout << text.substr(11) << endl; // הדפס "לדוגמה"

  return 0;
}

// פלט:
// זהו
// לדוגמה
```

```C++
// כתיב פונקציה משלכם:
#include <iostream>
#include <string>

using namespace std;

// הפונקציה מקבלת כפרמטר את המחרוזת המלאה ואת אורך התת-מחרוזת הרצויה
// ומחזירה את התת-מחרוזת:
string extractSubstring(string text, int start, int length) {
  string result = "";

  // הוספת התווים הרצויים לתת-מחרוזת:
  for (int i = start; i < start + length; i++) {
    result += text[i];
  }

  return result;
}

int main() {
  string text = "טקסט להסבר על