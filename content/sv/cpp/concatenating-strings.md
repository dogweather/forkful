---
title:                "C++: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Varför
I C++ programmering är det ofta nödvändigt att sammanfoga flera textsträngar till en enda sträng. Detta kan vara användbart för att skapa dynamiska meddelanden eller för att enkelt manipulera text.

## Hur du gör det
För att sammanslå strängar i C ++ kan du använda operatorn "+" eller funktionen "concatenate". Här är ett exempel på hur du sammanslår två strängar "Hello" och "World" och skriver ut resultatet:
```C++
#include <iostream>
using namespace std;

int main() {
    string str1 = "Hello";
    string str2 = "World";
    string result = str1 + " " + str2;
    //eller string result = str1.concatenate(" ").concatenate(str2);
    cout << result; //output: Hello World
    return 0;
}
```
## Djupdykning
När du sammanslår strängar i C ++ är det viktigt att förstå hur datatypen "string" fungerar. Eftersom strängar är sekvenser av tecken, är det enklaste sättet att sammanslå dem genom att helt enkelt lägga till tecknen till den ursprungliga strängen. Men det kan bli problematiskt om du behöver hantera specialtecken som till exempel mellanslag eller gåsighetstecken. I sådana fall behöver du använda escape-sekvenser eller konvertera de specialtecknen till sin ASCII-kod innan du sammanslår dem.

## Se även
- [C++ Strings Library](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [C++ Escape Sequences](https://www.javatpoint.com/cpp-escape-sequences)
- [ASCII Table](https://www.asciitable.com/)