---
title:                "עובדים עם CSV"
html_title:           "C++: עובדים עם CSV"
simple_title:         "עובדים עם CSV"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
שימוש בפורמט הקובץ CSV הוא דרך נפוצה לאחסון והעברת נתונים בין יישומים שונים. תוכנתנים משתמשים בו כדי לקרוא ולכתוב לקבצים CSV בכדי לטפל במידע מאורגן בפשטות.

## איך לעשות:
הנה דוגמה פשוטה של כיצד לכתוב נתונים לקובץ CSV בשפת C++:

```C++
// כותבים את כל ההכרזות שנדרשות לפני תחילת התכנית
#include <fstream> // ספריה שמאפשרת את העבודה עם קבצים
#include <iostream> // ספריה שאחראית על הקלט והפלט של הנתונים

using namespace std;

int main() {
    // נפתח קובץ חדש לכתיבה
    ofstream outfile;
    
    // נפתח את הקובץ ונכתוב את הנתונים
    outfile.open("data.csv");
    outfile << "מספר פריט, שם, מחיר" << endl;
    outfile << "1, מנעול, 50.00" << endl;
    outfile << "2, מפתח, 10.00" << endl;
    outfile.close(); // סגירת הקובץ
    
    // הדפסת הפלט למסך
    cout << "נתונים נכתבו בהצלחה לקובץ CSV!" << endl;
    return 0;
}

```

הנה נתוני הפלט שיופיעו בקובץ:

```
מספר פריט, שם, מחיר
1, מנעול, 50.00
2, מפתח, 10.00
```

## להעמיק:
פורמט הקובץ CSV (Comma Separated Values) נוצר במגזר התעשייתי כדי לאפשר חלוקת נתונים בקלות בין יישומים שונים. אחת היתרונות שלו הוא שהוא קל מאוד לנתח ולעבד את המידע המכיל.

אם אתם עובדים עם נתונים מורכבים יותר, כמו טבלאות, אפשר להשתמש בפרמטים כמו JSON או XML.

## ראה גם:
למידע נוסף על פורמט הקובץ CSV ואיך לקרוא ולכתוב לקובץ בשפת C++, ניתן לעיין במדריך המפורט של Microsoft כאן: https://docs.microsoft.com/en-us/cpp/standard-library/formatting-parsing.reading-and-writing-files.

בנוסף, אתר זה מכיל מידע נוסף על פורמטי קבצים נפוצים נוספים וכיצד לעבד אותם בקלות: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON.