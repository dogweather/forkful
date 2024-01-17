---
title:                "יצירת מספרים אקראיים"
html_title:           "C++: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
שחושבים על מספרים אקראיים, רובנו מתארים לעצמם סדרה של מספרים שמתבצעת באופן בלתי חוקי או לא צפוי. למה מתקיימת הצורך לייצר מספרים כאלו? מספרים אקראיים הם חשובים כי הם משמשים ככלי חשוב בתחום התכנות והמחשוב, מאפשרים לנו ליצור אלגוריתמים חכמים ולבצע צעדים עקביים בלתי צפויים.

## איך לעשות?
כדי לייצר מספרים אקראיים בשפת C++ ישנם כמה אפשרויות. נדגים כמה מהאפשרויות הנפוצות ביותר:

### שיטת rand
תחילה, ניצור משתנה מסוג int שנקרא num ונשתמש בפונקציה rand כדי לייצר מספר אקראי. פונקצית rand מחזירה מספר שלם בין 0 ל־RAND_MAX המוגדר לקלטים אקראיים במחשב. כדי לקבל מספר בין מספרים שונים נעטוף את הפונקציה rand עם המודולו (%) המחזיר את השארית בין שני מספרים.

```C++
int num = rand() % 10; // יצירת מספר אקראי בין 0 ל־9
```

### שיטת random
שיטת random היא בעלת יתרון על שיטת rand, משום שהיא משתמשת בווריאציות בתוך הקוד ליצירת מספרים אקראיים יותר. כדי להשתמש בשיטת random נצטרך לכלול את הספרייה <random> ולשים לב לפונקציית initalize (שנאפשר לנו לקבל זרע רנדומלית). כאן ניצור מספר אקראי בין 0 ל־9:

```C++
// ייבוא ספרייה <random>
#include <random>

int main() {
  // אתחול הנגרים עם הזרע הרנדומלי האפשרי
  std::mt19937 engine{std::random_device{}()};
  std::uniform_int_distribution<int> dist{0, 9};
  // לעשות אמת! נפעיל את המנוע הרנדומלי ונראה את התוצאה
  std::cout << dist(engine);
  return 0;
}
```

## מעמקים
תוכלו למצוא מידע נוסף על מחולל מספרים אקראיים ושימושם בתחום התכנות במקורות הבאים:

- [תיעוד על rand של C++](https://cplusplus.com/reference/cstdlib/rand/)
- [תיעוד על random של C++](https://cplusplus.com/reference/random/)
- [ספריית random במדריך של C++](https://tutorials.jenkov.com/cpp-standard-library/random.html)
- [מאמר על מחוללי מספרים אקראיים ושימושם בתחום התכנות](https://medium.com/@hobosapien/why-random-number-generators-are-important-for-programming-cd539d22f9ec)

## ראו גם
אם אתם מתעניינים בתחום ההיסטוריה של מחוללי מספרים אקראיים, תוכלו לקרוא עוד במאמרים הבאים:

- [היסטוריה של המחוללים האקראיים](https://blogs.mathworks.com/cleve/2018/01/08/random-number-generators-part-one-the-history/)
- [5 מחוללי מספרים אקראיים מפוצצי מוח ששונו את