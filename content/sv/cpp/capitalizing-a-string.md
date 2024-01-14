---
title:    "C++: Att stor-bokstavera en sträng"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Varför

Att använda sig av en funktion för att göra en sträng (string) stor och bokstavlig är ett vanligt förekommande problem inom programmering. Det kan vara bra att känna till och använda sig av denna funktion för att öka läsbarheten och stilriktigheten i koden.

## Hur man gör det

För att använda sig av funktionen för att göra en sträng stor och bokstavlig behöver man först skapa en variabel som håller strängen. Sedan kan man använda sig av "std::toupper" funktionen som är standard inom C++. 

```C++
std::string str = "hej, jag är en sträng!";
std::transform(str.begin(), str.end(), str.begin(), [](unsigned char c){ return std::toupper(c); });
std::cout << str; // Output: HEJ, JAG ÄR EN STRÄNG!
```

## Fördjupning

Funktionen "std::transform" tar in tre parametrar, där den första är startpunkten för strängen, andra är slutpunkten för strängen och den tredje är en funktion som tar in en variabel av typen "unsigned char" (vilket är varje enskild bokstav i strängen) och returnerar den stora och bokstavliga motsvarigheten till "c". I exemplet använder vi en lambda-funktion för att enklare returnera rätt värde.

En annan viktig sak att notera är att "std::transform" inte ändrar på originalsträngen, utan istället skapar en kopia av den med de önskade ändringarna. Detta gör det lättare att återanvända den ursprungliga strängen i programmet.

# Se även

- [C++ toupper() function](https://www.geeksforgeeks.org/toupper-function-in-cpp/)
- [std::transform documentation](https://en.cppreference.com/w/cpp/algorithm/transform)