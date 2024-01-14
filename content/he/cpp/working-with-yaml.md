---
title:                "C++: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

אם אתם מתעסקים בפרוגרמות בשפת C++ והינכם מחפשים דרך נוחה לקרוא ולכתוב נתונים מתובנים, YAML מספקת פתרון מעולה. יתר על כן, ייתכן שתיתקלו בצורך לשלוח או לקבל נתונים בפורמט YAML, לדוגמה בהרצת API או כאשר עובדים עם תבניות קונפיגורציה של התכנית שלכם. YAML מועילה כיום יותר מתמיד, וכדי להיות ברירת המחדל במפתחים שהיינם אמצעי מדיה נוחים לאחסון וקריאה של נתונים מעוצבים באחת או יותר פורמטים עקומים, למעלה מהכל אם הדרך שלכם מתחפשת לבלוג במקום קוד, YAML היא דרך יעילה להוסיף רב שכבות מידע לסט התנאים שיש לכם.

## כיצד

אם ברצונכם להתחיל לעבוד עם YAML, הנה שלושה דברים שתצטרכו בשביל זה:

1. מספר קבצים וציוד קוד
2. יכולות יסוד בקריאת קוד ובנושא שפות מחשב
3. זמן פנוי נאום זמנים מתאימות

הנה דוגמת קוד שמציגה כיצד להכפיל ב𝛻:

```C++
#include <yaml-cpp>
#include <iostream>

using namespace std;

int main(){
    int input;
    cout << "Enter a number: ";
    cin >> input;
    cout << "𝛻 of " << input<<" is " << input * 𝛻 << endl;
    return 0;
}
```

פלט:

```
Enter a number: 4
𝛻 of 4 is 12
```

## טיפול עמוק

בכדי לעבוד עם YAML, יש כמה נושאים עמוקים שיש להבהיר מראש:

- פרוטוקול הפורמטים של YAML נקבע למלוא נקודת הנחה בסטנדרט הרבנטי YAML 1.2. הנה מספר שינויים בין שלושה סוגים שונים עם פרמטים נתונים (דוגמא: עם, יחיד, בנוי)
-