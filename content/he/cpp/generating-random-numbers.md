---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

### מה ולמה?
יצירת מספרים אקראיים היא תהליך שבו ניצר מספרים שאין ביניהם קשר בלתי תלוי. מתכנתים מניחים את השימוש בזה במרבית המקרים להפוך את התהליכים ללא תנאיים או לאימות בדיקה.

### כיצד
אז איך אנחנו מייצרים מספרים אקראיים ב- C++? הנה דוגמא:
```C++
#include <random>  
#include <iostream>

int main() 
{
    std::random_device rd; 
    std::mt19937 gen(rd()); 
    std::uniform_int_distribution<> dis(1, 6); 

    for (int n = 0; n < 10; ++n) 
        std::cout << dis(gen) << ' '; 
    std::cout << '\n';
}
```

בדוגמה זו, המספרים אקראיים הנוצרים הם בין 1 ל- 6 וכל פעם מייצרים 10 מספרים.
הפלט יהיה משהו כמו:
```
5 2 4 1 6 3 3 4 2 6
```

### בהקשר הרחב
יצירת מספרים אקראיים היא תופעה שהיתה חלק מתכנות המחשבים מאז התחלתם. מאז, חוזרים המתכנתים לשימוש בה כדי להיישם את הרעיונות מעשיים שלהם. בינתיים, המגוון של שיטות נרחב. רקדאן הוא אחד מהשיטות הפופולריות ביותר ממוטראן - שיטה שעומדת בסטנדרטים של אקראיות גבוהים ביותר. אבל, * srand * ו- * rand *, אלטרנטיבות קלאסיות, משמשות עדיין ולעתים מעדיפים אותן בשל הפשטות שלהם. אם אתה מעוניין לדעת יותר, להוסיף מנגנון של אקראיים לתוכנה שלך.

### עיין גם
1. [C++ ספרייה אקראית](https://en.cppreference.com/w/cpp/numeric/random)
2. [Mersenne Twister, ויקיפדיה](https://en.wikipedia.org/wiki/Mersenne_Twister)
3. [ספרייה של C rand](https://www.cplusplus.com/reference/cstdlib/rand/)