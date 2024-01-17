---
title:                "Att Göra en Sträng Stor Bokstav"
html_title:           "C++: Att Göra en Sträng Stor Bokstav"
simple_title:         "Att Göra en Sträng Stor Bokstav"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att kapitalisera en sträng betyder att göra den första bokstaven i varje ord stora. Programmmatörer gör detta för att göra strängar mer läsliga och tydliga.

# Hur man:
Här är ett exempel i C ++ om hur man kapitaliserar en sträng:

```C++
#include <iostream>
#include <string>

using namespace::std;

int main() {
    string str = "hej på dig!";
    for(int i = 0; i < str.size(); i++) {
        if(i == 0) {
            str[i] = toupper(str[i]);
        }
        else if(str[i - 1] == ' ') {
            str[i] = toupper(str[i]);
        }
    }
    
    cout << str << endl;
    
    return 0;
}
```

Output:
```
Hej På Dig!
```

# Djupdykning:
Historiskt sett användes kapitalisering för att markera början av en mening eller ett nytt namn. Idag används det främst för att göra text mer läslig för användaren. Istället för att använda en loop kan man också använda en inbyggd funktion i C++ som heter `capitalize()` för att göra samma sak.

# Se även:
- https://www.cplusplus.com/reference/string/string/capitalize/ - Inbyggd funktion för kapitalisering i C++.
- https://www.geeksforgeeks.org/capitalize-first-letter-of-every-word-in-a-given-string/ - Alternativ lösning för att kapitalisera en sträng.