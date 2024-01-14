---
title:                "C++: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

למה: ניתן למצוא את עצמך במצב שבו יש לך טקסט גדול ואתה רוצה לשנות חלק ממנו באופן מהיר ויעיל. בעזרת חיפוש והחלפה של טקסט בקוד C++, אתה יכול לבצע תפקיד זה בקלות ובצורה ממוקדת.

איך לעשות זאת: להלן דוגמאות של קודים ופלט כלשהו בתוך בלוקי קוד "```C++ ... ```".

```C++
// החלפת האות "א" לאות "ב" במחרוזת נתונה
string text = "אני אוהב C++";
text.replace("א", "ב");
cout << text << '\n';
// פלט: "בני בועב C++"

// מציאת והחלפת מחרוזות נתונות
string text = "תכנות הוא כיף";
size_t found = text.find("כיף");
if (found != string::npos) {
    text.replace(found, 3, "נהדר");
}
cout << text << '\n';
// פלט: "תכנות הוא נהדר"

// החלפת מחרוזת קיימת בשורות מרובות בקובץ
string line;
ifstream file("myfile.txt");
if (file.is_open()) {
    while (getline(file, line)) {
        size_t found = line.find("ישן");
        if (found != string::npos) {
            line.replace(found, 3, "חדש");
        }
        // ללא כתיבת הקובץ מחדש, אלא עריכתו על ידי קובץ זמני חדש
        ofstream temp("temp.txt", ios::app);
        temp << line << '\n';
    }
    file.close();
    // מחיקת הקובץ המקורי והחלפתו בקובץ הזמני
    remove("myfile.txt");
    rename("temp.txt", "myfile.txt");
}
```

עיון מעמיק: באמצעות פקודות כמו find, replace ומשתנים כמו string::npos ניתן להתאים ולהחליף חלקים נתונים של טקסט בקלות. כדי ליצור אפקטיביות וייעול רב יותר, ניתן להשתמש בפונקציות נוספות כמו regex_replace המציגה אפשרויות נרחבות יותר לחיפוש והחלפה של טקסט.

ראה גם: 
- [מדריך לשפת C++](https://www.geeksforgeeks.org/c-plus-plus/)
- [תיעוד רשמי של C++](https://devdocs.io/cpp/)
- [אתר חיפוש בקוד C++ מקוון