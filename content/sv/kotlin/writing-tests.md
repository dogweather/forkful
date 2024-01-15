---
title:                "Skriva tester"
html_title:           "Kotlin: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

### Varför skriva tester?
Att skriva tester är en viktig del av utvecklingsprocessen och hjälper till att säkerställa att vår kod fungerar som den ska. Genom att skriva tester kan vi i förväg upptäcka eventuella buggar och problem, vilket sparar tid och resurser i det långa loppet.

### Så här skriver du tester i Kotlin
För att skriva tester i Kotlin behöver du ett testramverk som JUnit eller Spek. När du har importerat ramverket kan du skapa en testklass och implementera dina tester med hjälp av olika metoder som `assume()`, `assertEquals()`, och `assertTrue()`. Nedan följer ett exempel på hur en testklass kan se ut i Kotlin.

```Kotlin
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import kotlin.test.assertEquals

@RunWith(JUnit4::class)
class CalculatorTest {

  @Test
  fun `test add()`() {
    val calculator = Calculator()
    val sum = calculator.add(2, 2)
    assertEquals(4, sum)
  }

  @Test
  fun `test divide()`() {
    val calculator = Calculator()
    val quotient = calculator.divide(10, 2)
    assertEquals(5, quotient)
  }
}
```

För att köra testerna, högerklicka på testklassen och välj "Run 'CalculatorTest'". Om alla tester passerar, så visas gröna markeringar bredvid varje testmetod. Om ett test misslyckas, kommer det att visas en röd markering tillsammans med information om vad som gick fel.

### Utforska tester djupare
Skrivandet av tester är en grundläggande del av testdriven utveckling (TDD), där man först skriver testerna för att sedan implementera funktionerna. Det ger en mer strukturerad och pålitlig kod, samtidigt som det hjälper till att hitta eventuella buggar tidigt i utvecklingsprocessen. För mer information om hur man använder olika testramverk och implementerar tester i Kotlin, rekommenderar jag att läsa mer på Kotlin's dokumentationssidor.

### Se även
- [Kotlin Test](https://kotlinlang.org/docs/tutorials/tdd-setup.html#kotlin-test)
- [JUnit API](https://junit.org/junit5/docs/current/api/)
- [Spek](https://spekframework.org/)