---
title:    "C++: השוואת שתי תאריכים"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מדוע
מתכנתים רבים מתחילים את המסלול שלהם בתוכניתן חישוב הזמן מתאר מהבסיס. תוכניתן חישוב הזמן מאפשרת למשתמשים להשוות בין שתי תאריכים, משהו שיכול להיות שימושי בהחלט כאשר מטרת התוכנית היא להשוות בין תאריכים שונים או לבדוק כמה זמן עבר מאז תאריך מסוים.

## כיצד לעשות זאת
בכדי לבצע השוואה בין שתי תאריכים בשפת סי++ ישנם כמה דרכים שונות. אחת הדרכים הבסיסיות היא על ידי בניית פונקצייה שתקבל שני תאריכים כפרמטרים ותחזיר תוצאה על סמך נתונים מפורטים. אפשר להשיג זאת בעזרת פעולות של סי++ כמו מצביעים ופונקציות מובנות. הנה דוגמה לקוד שמחזיר תוצאה של השוואה בין שני תאריכים:

```C++
#include <iostream>
using namespace std;

int compareDates(int* date1, int* date2) {
    if (date1[2] < date2[2]) 
        return 1; // date1 is earlier
    else if (date2[2] < date1[2]) 
        return 2; // date2 is earlier
    else {
        if (date1[1] < date2[1]) 
            return 1; // date1 is earlier
        else if (date2[1] < date1[1]) 
            return 2; // date2 is earlier
        else {
            if (date1[0] < date2[0]) 
                return 1; // date1 is earlier
            else if (date2[0] < date1[0]) 
                return 2; // date2 is earlier
            else 
                return 0; // dates are equal
        }
    }
}

int main() {
    int date1[3] = {12, 10, 2021}; // first element is day, second is month, third is year
    int date2[3] = {24, 12, 2020};
    int result = compareDates(date1, date2); // returns 2
    cout << "Result: " << result << endl;
    return 0;
}
```
מצפים לקבל כתובת חיסור הנתונים של הפונקציה. סופק על פי התייעצות עם [ ).

## צליל עמוק
ההשוואה בין תאריכים בסי++ יכולה להיות מעט מורכבת מתחילתו. המטרה של ביצוע השוואה בין