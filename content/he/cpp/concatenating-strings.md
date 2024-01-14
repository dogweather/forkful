---
title:                "C++: מחבר מחרוזות"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

מדוע: למה אנשים משתתפים בביצוע פעולת חיבור מחרוזות רק תיאור קצר (1-2 משפטים) של *למה* זה חשוב כל כך.

כיצד לבצע: דוגמאות קוד ופלט מתוך חלונות קוד "```C++ ... ``` ".

## למה

החיבור של מחרוזות הוא פעולה חשובה בתחום של תיכנות בשפת C++. הוא מאפשר למפתחים ליצור מחרוזות ארוכות יותר על ידי חיבור של מחרוזות קטנות יותר. פעולת חיבור מחרוזות גם עוזרת לנו לפעול עם מידע ולהציגה למשתמש בצורה יותר נוחה.

לפניכם דוגמאות של כמה שיטות לחיבור מחרוזות בתוך חלונות קוד:

```C++
// חיבור של שתי מחרוזות עם כוונות תוכנה

#include <iostream>
#include <string>
using namespace std;

int main() {
    string firstName = "טל";
    string lastName = "כהן";
    string fullName = firstName + " " + lastName;
    cout << "שלום, שמי הוא " << fullname << endl;
    // הפלט יהיה: שלום, שמי הוא טל כהן
    return 0;
}
```

```C++
// חיבור של מחרוזת ומספר

#include <iostream>
#include <string>
using namespace std;

int main() {
    string age = "20";
    string message = " לי יש " + age + " שנים";
    cout << message << endl;
    // הפלט יהיה: לי יש 20 שנים
    return 0;
}
```

הנה עוד דוגמא:

```C++
// חיבור של מערך של מחרוזות

#include <iostream>
#include <string>
using namespace std;

int main() {
    string cities[3] = {"תל אביב", "אילת", "חיפה"};
    string message = "אני מתכנן לבקר ב-" + cities[0] + " וב-" + cities[2];
    cout << message << endl;
    // הפלט יהיה: אני מתכנן לבקר בתל אביב ובחיפה
    return 0;
}
```

כפי שאתם רואים, חיבור מחרוזות מאפשר לנו ליצור מחרוזות ארוכות ומורכבות יותר על ידי חיבור של מחרוזות קטנות בהרכבה שונה. זה נותן לנו יכולת לעבוד עם מ