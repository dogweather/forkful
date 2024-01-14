---
title:    "C++: קריאת ארגומנטים בשורת פקודה"
keywords: ["C++"]
---

{{< edit_this_page >}}

# למה

במאמר זה, אנו נדבר על כיצד לקרוא טקסט משורת הפקודה בעזרת פקודת C++ "command line arguments". זהו נושא שימושי לכל מתכנת C++ שמעוניין ליצור תוכניות פותחות ונעימות לשימוש. אם אתה מתכנת שמעוניין לשפר את התכניות שלך או ללמוד דרך נוספת לכתיבת קוד נקי ויעיל, המאמר הזה מתאים בדיוק בשבילך.

# כיצד

כדי לקרוא טקסט משורת הפקודה, נצטרך להשתמש בפקודת C++ שנקראת "int main(int argc, char* argv[])". כמו כן, נציג גם שני דגימות של קוד C++ ואת הפלט שלהן.

```C++
#include <iostream>
using namespace std;

int main(int argc, char* argv[])
{
    cout << "מספר הפרמטרים הוא: " << argc << endl;
    for (int i = 0; i < argc; i++)
    {
        cout << "הפרמטר מספר " << i << " הוא: " << argv[i] << endl;
    }
}
```

פלט:

```
מספר הפרמטרים הוא: 3
הפרמטר מספר 0 הוא: program.exe
הפרמטר מספר 1 הוא: first_argument
הפרמטר מספר 2 הוא: second_argument
```

כפי שאתה רואה, הפרמטר הראשון (0) הוא שם התוכנית שנקראת, והפרמטרים הבאים הם הארגומנטים שהשתמשנו בהם כאשר הפעלנו את התוכנית.

# מעמד עמוק

עכשיו שאנחנו יודעים כיצד לקרוא פרמטרים משורת הפקודה, נעמיק קצת יותר ונלמד כיצד לעבוד עם אותם פרמטרים. על ידי השתמשות בספרייה הסטנדרטית <string>, נוכל לצרף את הארגומנטים למחרוזת אחת, וסופסוף לעבוד איתם כדרך אחת.

```C++
#include <iostream>
#include <string>
using namespace std;

int main(int argc, char* argv[])
{
    string arguments = "";
    for (int i = 1; i < argc; i++)
    {
        arguments += argv[i];
        if (i != (argc