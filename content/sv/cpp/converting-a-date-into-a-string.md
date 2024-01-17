---
title:                "Omvandla ett datum till en sträng"
html_title:           "C++: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera ett datum till en sträng är en vanlig uppgift som utförs av programmerare för att göra det möjligt att läsa och hantera datum på ett enklare sätt. Genom att göra detta kan man omvandla ett datums numeriska värden till en textrepresentation som är mer läsbar och tydlig för människor.

## Så här gör du:
```C++
#include <iostream> 
#include <sstream> 
#include <iomanip> 

int main() {
	// Skapa ett datumobjekt med dag, månad och år 
	int dag = 15; 
	int manad = 9; 
	int år = 2021; 

	// Skapa en ström för att konvertera datumet till en sträng
	std::ostringstream ss; 

	// Ange formatet för datumet 
	ss << std::setfill('0') << std::setw(2) << dag << "/" << std::setw(2) << manad << "/" << år;

	// Spara den konverterade strängen i en variabel
	std::string datumStr = ss.str(); 

	// Skriv ut resultatet 
	std::cout << "Datum som sträng: " << datumStr << std::endl; 

	return 0; 
}
```

Output:
```C++
Datum som sträng: 15/09/2021
```

## Djupdykning:
Konverteringen av datum till strängar har varit en utmaning för programmerare sedan tidiga dagar av programmering. Innan standardiseringen av språk som C++, var det upp till varje programmerare att hitta en lösning på denna utmaning. Idag finns det emellertid inbyggda funktioner och bibliotek som gör det möjligt att konvertera datum till strängar på ett enklare och mer effektivt sätt.

Det finns också alternativa sätt att konvertera datum till strängar, som att använda delningsoperatorn (%) eller att använda en char-array. Dessa metoderna kan dock vara mer komplexa och mindre flexibla än den som presenteras i exemplet ovan.

Den här konverteringsprocessen är också viktig för att kunna spara datum i en databas eller skriva ut det på olika språk och tidszoner. Det är därför viktigt för programmerare att behärska denna färdighet för att kunna hantera och hantera datum på ett effektivt sätt.

## Se även:
1. [http://www.cplusplus.com/reference/ctime/strftime/](http://www.cplusplus.com/reference/ctime/strftime/)
2. [https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
3. [https://en.wikipedia.org/wiki/C_date_and_time_functions](https://en.wikipedia.org/wiki/C_date_and_time_functions)