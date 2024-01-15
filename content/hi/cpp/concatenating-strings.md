---
title:                "स्ट्रिंग संयोजन करना"
html_title:           "C++: स्ट्रिंग संयोजन करना"
simple_title:         "स्ट्रिंग संयोजन करना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyon
Kya aapne kabhi strings ko concatenate kiya hai? Yadi haan, to aapko shayad pata hoga ki yeh ek bahut hi upyogi aur zaruri kaam hai. Concatenating strings ke madhyam se hum ek lamba string bana sakte hai jo dusre strings ko jodkar banta hai. Isse hume alag-alag strings par alag-alag operations karne ki zarurat nahi padti hai.

## Kaise Kare
```
#include<iostream> 
using namespace std; 
  
int main() 
{ 
    // Sabse pehle hum do strings define karenge 
    string s1 = "Hello"; 
    string s2 = "World"; 
  
    // Ab hum inhe jodkar ek naya string banayenge 
    string result = s1 + s2; 
  
    // Ab hum result string ko print karenge 
    cout << result; 
    // Output: HelloWorld
  
    return 0; 
} 
```

Is code mein, humne pehle do strings ko define kiya hai. Fir humne inhe concatenation ke operator '+' ka istemaal karke joda hai. Isse ek naya string ban gaya hai jo "HelloWorld" hai. Isse hume result string mein dono strings ko jodkar ek hi statement mei likhna padta hai.

## Gehri Jhanjhod
Strings ko concatenate karne ke liye C++ mein do tarike hai - '+' operator ka istemaal karna ya phir 'append()' function ka istemaal karna. Dono tarike ka output same hoga lekin unke piche ka process thoda alag hai.

### '+' operator
Yeh operator strings ko jodta hai aur ek naya string bana deta hai. Isse hum multiple strings ko bhi jod sakte hai. Yadi humne string variables ko concatenate kiya hai to hume naye variable mein result store karna padega kyunki isse pehle variable mei koi change nahi hoga.

### append() function
Yeh function strings ko jodkar original string mei changes karta hai. Matlab ki isse humme naye variable mei store karne ki zarurat nahi padti hai. Iska syntax is prakar hota hai - ```string_variable.append(another_string)```.

## Dekhein bhi
- [C++ Strings](https://www.programiz.com/cpp-programming/string)
- [C++ Operators](https://www.programiz.com/cpp-programming/operators)