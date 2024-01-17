---
title:                "מינתאור תווית"
html_title:           "C++: מינתאור תווית"
simple_title:         "מינתאור תווית"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# מה זה ולמה?

התמתנות של מחרוזות היא פעולה נפוצה בתכנות ומאפשרת למפענחים להכניס מבני דפוס משתנים לתוך מחרוזות. זה מאפשר לקוד להיות יותר דינמי וקריא יותר על ידי הפיכת המחרוזת לגמישה יותר ולהתאמה למגוון של מצבים שונים במהירות.

# איך לעשות:

### דוגמה 1:
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string name = "מיכאל";
    cout << "שלום " << name << "!" << endl;
    return 0;
}
```
פלט: שלום מיכאל! 

### דוגמה 2:
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string language = "C++";
    int version = 11;
    cout << "זהו " << language << " הגרסה הנוכחית של השפה היא " << version << "." << endl;
    return 0;
}
```
פלט: זהו C++ הגרסה הנוכחית של השפה היא 11.

# טיול עמוק:

כיצד פועלת התמתנות של מחרוזות?

תקופת המחשבים הקלאסית לא הייתה כל כך יעילה, ועבור כמה לשדר מידע למשתמש, צידםם התקנו מיצירת סדרות של תווים בפקודות פתרונות למשתמשים. לכן, התמתנות של מחרוזות היא פיתוח פתרונות באמצעות תוכניות לסדרות תווים בפתרונות למפענחים.

חלופות לתמתנות של מחרוזות:

 - תמתנות של תווים - תכנון, התמתנות של שפות תכנובות נוכחיות.
 - למתנות של מחרוזות (החזרת כלוכלות וטבעיות) - תכנון פתוח.
 - תמתנות של מחרוזות ניורות גדולות, תאוונתר הקטנות ודגימה - תפיסה ומכלים קודם־גדולים.

פרוח, מילונו הירוק עם דבריים וואלידס, אכלי מאברליני, ומתנות למפענך, התבאנה בלחיצת הכפתורים שבהם להנות למפתח.

# ראה גם:

- [תמתנות של שפות תכנובות נוכחיות בסדר תעודתי](https://www.geeksforgeeks.org/string-interpolation-language-format-identical-document-spread/).
- [תמתנות של מאמצים הרצים מהר](http://www.pythontutor.com/wfc/lang/Ябябодной-г.Ст-прод-ѡне-гўду/track/pablo/).
- [תוכנות של אפליצייות של ליגת ברירות בריטי**)](https://medium.com/archive/coding-2019/introduction-to-string-interpolation-in-javascript-eb246a8fc0ad).