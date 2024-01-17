---
title:                "Skriva tester"
html_title:           "Java: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi pratar om att skriva tester i programmering så menar vi att skapa en uppsättning instruktioner som testar koden för vårt program och kontrollerar att det fungerar som det ska. Detta är viktigt eftersom det hjälper oss att hitta och fixa buggar och fel i koden innan vi släpper den till användare.

## Så här:
För att skriva tester i Java behöver vi använda ett ramverk som heter JUnit. Detta låter oss skapa olika tester för olika delar av koden och köra dem automatiskt för att kontrollera att allt fungerar som det ska.

```Java
public class CalculatorTest {

  @Test
  public void testSum() {
    int result = Calculator.sum(3, 7);
    assertEquals(10, result);
  }
  
  @Test
  public void testDivision() {
    double result = Calculator.divide(10, 2);
    assertEquals(5.0, result);
  }

}
```

Testerna lägger vi sedan i en egen testmapp inuti vårt projekt och kör dem med hjälp av en IDE eller genom att skriva "mvn test" i terminalen. Om något test inte lyckas betyder det att vi behöver gå tillbaka till vår kod och fixa det som inte fungerar.

## Deep Dive:
Historiskt sett har skrivande av tester ofta sett som en tidskrävande process, men med JUnit och andra ramverk har processen blivit automatiserad och mycket snabbare. Det finns även andra alternativ för att skriva tester, såsom TestNG och Mockito, men JUnit är det mest populära valet inom Java-världen.

Det finns många olika metoder för att skriva tester, men ett vanligt sätt är att använda en teknik som kallas "test-driven development" (TDD). Detta innebär att vi skriver våra tester innan vi skriver själva kod, vilket hjälper oss att fokusera på vad vår kod faktiskt ska göra och hur vi ska testa det.

## Se även:
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [TestNG](https://testng.org/doc/index.html)
- [Mockito](https://site.mockito.org/)
- [Test-driven development (Wikipedia)](https://en.wikipedia.org/wiki/Test-driven_development)