---
title:                "स्ट्रिंग को लोअर केस में रूपांतरण करना"
html_title:           "C++: स्ट्रिंग को लोअर केस में रूपांतरण करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरण करना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyun
Mool roop se, yaani ke original form mein likhe to excel, programming languages, aur databases mein bhi uske istemaal ke kai takiye hai. Dhyan rakhna chahiye ki capital aur small letters mein farak hota hai. Kuch cases mein, humein string ko lower case mein convert karna padta hai taaki sahi se compare ya search kar sakein.

## Kaise Karein
```C++
#include <iostream>
#include <string>
#include <locale>

using namespace std;

int main(){
    string input;
    cout << "Kripya string enter karein: ";
    getline (cin, input); // input lein
    for(int i = 0; i < input.length(); i++) {
        // Har character ko lower case mein convert karein
        input[i] = tolower(input[i], locale());
    }
    cout << "Converted string: " << input << endl;
    return 0;
}
```
### Output:
```
Kripya string enter karein: HELLO WoRlD
Converted string: hello world
```

## Gehri Jhaank
Jab bhi hum string ko lower case mein convert karte hai, hum ek loop ke through har character ko check karte hai aur use `tolower()` function se convert karte hai. Iske baad, hum converted string ko output karte hai. Ek important baat hai ki `tolower()` function `locale` object ka argument leta hai jis se hum character ko sahi language mein convert kar sakein. Agar hum `locale()` nahi use karein, to character ko `char` type mein convert kar di jayegi. Isse humare code mein kuch characters jaise ki 'ç' ya 'ß' ko theeek tarah se convert nahi kar payega.

## See Also
- [String Functions in C++](https://www.geeksforgeeks.org/string-class-in-c/) 
- [Locale Object in C++](https://www.geeksforgeeks.org/locale-class-in-c-with-examples/)