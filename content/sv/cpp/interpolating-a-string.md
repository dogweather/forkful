---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Stränginterpolering är processen att infoga variabler i en sträng. Detta gör det lättare att arbeta med strängar och förhindrar programmeraren från att skriva extra kod för att sammanfoga strängar.

## Hur man gör:

Stränginterpolering i C++ kan göras med `std::ostringstream` biblioteksfunktionen så här:

```C++
#include <sstream>
#include <string>
#include <iostream>

int main() {
    std::ostringstream ss;
    int age = 20;
    std::string name = "Olle";
    ss << "Hej, jag heter " << name << " och jag är " << age << " år gammal.";
    std::cout << ss.str() << std::endl;
    return 0;
}
```

Detta skriver ut:
```
Hej, jag heter Olle och jag är 20 år gammal.
```

## Djupdykning

Stränginterpolering har varit ett vanligt koncept inom programmering sedan länge och är tillgängligt i många språk. I C++ har vi alternativ som `sprintf`, men dessa kan vara riskabla på grund av buffertspill. En annan metod är `fmt` biblioteket som ger en Python-liknande syntax.

Interpolering fungerar genom att generera en sträng med plats för variabler och sedan ersätta dessa delar med riktiga värden. Detta kräver att tillräckligt minne är reserverat, annars kan vårt program krascha eller bete sig oönskat.

## Se även

- Officiell dokumentation för [`std::ostringstream`](http://www.cplusplus.com/reference/sstream/ostringstream/)
- [`fmt`](https://fmt.dev/latest/index.html) biblioteket för modern strängformatering
- Hur stränginterpolering fungerar i andra språk som [Python](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting) och [JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)