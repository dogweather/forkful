---
title:    "C++: दो दिनांकों की तुलना करना"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

"## Kyun: Do tareekhon ko tulna karne mein kyun samay bitana chahiye?"

Do tareekhon ko tulna karne mein samay bitana aam baat hai, aur yeh desh mein chalne wale har vyakti ke liye zaroori ho sakta hai. Date comparison, humein do tareekhon ke beech mein antar ka pata lagane mein madad karta hai, jaise bharat sarkar dvaara jaari kiye gaye praman patron ko samajhne mein. Iske alawa, date comparison humein kisi bhi samay ke baare mein jaankari pradaan karta hai, jaise ki kitne din, mahine ya saal pahle koi ghatna ghatit hui thi. Isliye, do tareekhon ko tulna karne ke liye ek vyakti ko programming mein samay bitana bahut zaroori hai.

"## Kaise Kare: C++ mein do tareekhon ko tulna kaise karein?"

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Pahle tareekh ko decide karein
    time_t t1 = time(0); 

    // Dusra tareekh ko decide karein
    struct tm * now = localtime( & t1 ); 
    int year = now -> tm_year + 1900;
    int month = now -> tm_mon + 1;
    int day = now -> tm_mday;

    // Tareekhon ko tulna karein
    t2 = mktime( & now);

    // Calculated antar ko pradaan karein
    cout << "Antar: " << difftime(t2, t1) << " seconds." << endl;

    return 0;
}
```

"## Gehri Jhanjhavat: Do tareekhon ko tulna karne ki aur gehri jankari"

Do tareekhon ko tulna karna koi mushkil kaam nahi hai, lekin usmein kuch zaroori baaton ka dhyan rakhna hai. Pahle, humein tareekhon ko sahi format mein store karna zaroori hai. Doosre, hume pata hona chahiye ki nirdharit tareekh kis time zone mein hai. Tisra, pata karna hai ki computer ka default time zone kya hai, kyunki date comparison time zone ke hisab se hota hai. Aur aakhir mein, sahi function ka istemaal karna zaroori hai tareekhon ko tulna karne ke liye.

"## Dekhiye Bhi:"

- https://www.geeksforgeeks.org/how-to-find-difference-between-two-dates-in-c-programming-language/
- https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm
- https://www.programiz.com/cpp-programming/library-function/ctime/time