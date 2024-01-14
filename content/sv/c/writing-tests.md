---
title:                "C: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmering, särskilt i C. Det hjälper till att säkerställa att koden fungerar som det är tänkt och minskar risken för buggar och fel. Det är också ett effektivt sätt att kontrollera huruvida koden följer specifikationerna och förbättra kvaliteten på koden.

## Hur man gör

För att skriva tester i C, börja med att definiera en funktion som du vill testa. I följande exempel ska vi testa en funktion som beräknar medelvärdet av två tal.
```C
// Funktion för att beräkna medelvärde av två tal
double calculateAverage(double num1, double num2) {
    return (num1 + num2) / 2.0;
}
```
Nu behöver vi skapa en testfunktion som kan verifiera att resultatet av calculateAverage är korrekt.
```C
// Funktion för att testa calculateAverage
void testCalculateAverage() {
    // Testa första scenariot
    if (calculateAverage(10, 20) == 15) {
        printf("Test 1 lyckades!\n");
    } else {
        printf("Test 1 misslyckades!\n");
    }

    // Testa andra scenariot
    if (calculateAverage(5, 9) == 7) {
        printf("Test 2 lyckades!\n");
    } else {
        printf("Test 2 misslyckades!\n");
    }
}
```
Nu kan vi köra vår testfunktion och se resultatet i terminalen.
```C
int main() {
    testCalculateAverage();
    return 0;
}
```
Output:
```
Test 1 lyckades!
Test 2 lyckades!
```
Som vi kan se visar vår testfunktion att calculateAverage fungerar korrekt i båda scenarierna.

## Djupdykning

Det finns flera fördelar med att skriva tester i C. Förutom att förbättra kvaliteten på koden och minska risken för buggar, så kan tester också fungera som dokumentation för koden. De ger också en möjlighet att identifiera och lösa problem i ett tidigt skede under utvecklingsprocessen, vilket kan spara tid och resurser i det långa loppet.

När du skriver tester är det också viktigt att tänka på testets täckning. Detta innebär att se till att alla delar av koden är täckta av tester för att minska risken för missade buggar och fel. Det finns också olika typer av tester som kan användas i C-programmering, som enhetstester, integrationstester och funktionella tester.

Slutligen, för att uppnå bästa möjliga resultat med tester, se till att skriva testbara och modulära funktioner som enkelt kan testas och som har klara specifikationer för förväntat beteende och output.

## Se även

- https://www.geeksforgeeks.org/integration-testing/
- https://www.tutorialspoint.com/software_testing_dictionary/test_coverage.htm
- https://www.tutorialspoint.com/software_testing_dictionary/unit_testing.htm