---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:48:59.832474-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת מספרים אקראיים במחשב זו אומנות די מתחכמת בה המחשב 'מחליט' על מספר שנראה לנו לא צפוי. מתכנתים עושים זאת לכל מיני סיבות, כולל משחקים, סימולציות ואבטחת מידע.

## איך לעשות:

```C++
#include <iostream>
#include <random>

int main() {
    // יוצרים זרם פסאודו-אקראי חדש
    std::random_device rd;
    // מאתחלים מחולל מספרים אקראיים
    std::mt19937 gen(rd());
    // מגדירים את התחום של המספרים שאנחנו רוצים
    std::uniform_int_distribution<> distr(1, 100);

    // יוצרים מספר אקראי
    int random_number = distr(gen);
    // מדפיסים את המספר האקראי
    std::cout << "Random number: " << random_number << std::endl;

    return 0;
}
```

פלט לדוגמה:
```
Random number: 42
```

## נפילה חופשית:

בעבר, מחוללי המספרים האקראיים שימשו נוסחאות פשוטות יחסית, אבל עם זאת הם לא היו "אקראיים אמיתיים". במחשבים מודרניים יש שימוש במחוללי מרקוב כמו `std::mt19937`, שהוא קצר יחסית ומספק רמה גבוהה של חוסר צפיות. חשוב לזכור שתמיד יש עיקרון שלא יכול להתקיים אקריות אמיתית במחשב – זו תמיד תהייה "פסאודו-אקראית". התוצאות אמנם נראות לנו אקראיות, אבל אם נודע לנו את המצב ההתחלתי, ניתן לחזות את הסדרה שתווצר.

## להעמיק יותר:

- [cppreference.com - Random number generation](https://en.cppreference.com/w/cpp/numeric/random)
- [Wikipedia - Pseudo-random number generation](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Wikipedia - Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)