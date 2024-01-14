---
title:    "PHP: Skriva tester"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del i utvecklingsprocessen för alla programvaror. Tester hjälper till att säkerställa att koden fungerar som den ska och minskar risken för buggar och felaktigheter. Det kan också bidra till att öka kvaliteten på koden och underlätta underhållning av programvaran på lång sikt.

## Hur man gör det

För att skriva tester i PHP, behöver du använda ett testramverk som till exempel PHPUnit. Med dessa ramverk kan du skapa tester för din kod genom att definiera olika förväntade resultat och jämföra dem mot det faktiska resultatet. Här nedanför finns ett enkelt exempel på hur man skriver en testmetod för att kontrollera om en given array innehåller ett specifikt värde:

```PHP
function testContainsValue() {
  $array = [1, 2, 3, 4, 5];
  $value = 3;

  $this->assertContains($value, $array);
}
```

I detta exempel använder vi funktionen `assertContains`, som är en del av PHPUnit-ramverket. Den jämför värdet som förväntas finnas i arrayen mot det faktiska resultatet.

## Djupdykning

Att skriva tester handlar om mer än bara att kontrollera att koden fungerar som den ska. Genom att använda olika typer av tester kan du också förbättra designen och strukturen hos din kod. Till exempel kan enhetstester hjälpa till att hålla koden modulär och lättare att förstå och ändra.

En annan fördel med att skriva tester är att det underlättar vid buggfixar och uppdateringar av koden. Om något skulle gå fel i din programvara, kan du snabbt isolera problemet genom att köra tester och hitta den specifika delen av koden som behöver åtgärdas.

## Se även

För mer information om att skriva tester i PHP, se följande länkar:

- [PHPUnit dokumentation](https://phpunit.de/documentation.html)
- [Enhetstester vs integrations- och funktions tester](https://medium.com/@mknudsen01/unit-tests-vs-integration-tests-vs-functional-tests-22f738bf8b21)
- [Enkelheten i enhetstester](https://www.toptal.com/qa/how-i-learned-to-write-unit-tests-in-php)

Lycka till med att integrera tester i din utvecklingsprocess!