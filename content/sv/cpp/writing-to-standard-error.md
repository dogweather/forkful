---
title:    "C++: Skrivande till standardfel"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel (standard error) är en viktig del av felsökning och feldiagnostik i C++ programmering. Genom att skriva till standardfel kan du skicka meddelanden om eventuella fel eller undantag som uppstår i ditt program, vilket hjälper till att felsöka och förbättra kvaliteten på din kod.

## Så här

För att skriva till standardfel i C++ behöver du använda "std::cerr" funktionen. Detta är en ström som är kopplad till konsolen och används för att skriva ut felmeddelanden. Här är ett enkelt exempel på hur du skulle använda den:

```C++
#include <iostream>

int main()
{
  int a = 5;
  int b = 0;
  
  // Dividera a med b och skriv ut eventuellt felmeddelande
  if (b == 0)
  {
    std::cerr << "Kan inte dividera med noll!" << std::endl;
  }
  else
  {
    std::cout << "Resultatet är: " << a / b << std::endl;
  }
  
  return 0;
}
```

Detta kodblock visar hur du kan använda standardfel för att hantera felaktiga inmatningar och undvika kraschar i ditt program. Meddelandet "Kan inte dividera med noll!" kommer att skrivas ut till standardfel och programmet kommer att fortsätta köras utan att krascha.

## Djupdykning

Det finns flera andra sätt att använda standardfel i C++, inklusive skriva till en loggfil, omrikta standardfel till en annan ström eller hantera flera felmeddelanden i en try-catch-block. Det är också möjligt att anpassa standardfelmeddelanden med hjälp av "std::setbuf()" funktionen.

Det är viktigt att komma ihåg att använda standardfel är ett kraftfullt verktyg, men det bör bara användas för felsökning och diagnostik. Det är inte bra att använda standardfel för att kommunicera med användare av ditt program, eftersom meddelandena kan vara svåra att förstå för icke-tekniska personer.

## Se även

- [C++ Standardbibliotek](https://sv.wikipedia.org/wiki/C%2B%2B_standardbibliotek)
- [Felhantering i C++](https://www.cplusplus.com/doc/tutorial/exceptions/)
- [Guide till C++ programmering](https://www.learncpp.com/)