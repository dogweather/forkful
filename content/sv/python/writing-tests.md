---
title:    "Python: Att skriva tester"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en avgörande del av att utveckla programvara. Det hjälper till att kontrollera att ens kod fungerar korrekt och undvika buggar som kan orsaka problem i framtiden. Det sparar också tid och resurser genom att tillåta snabbare upptäckt och åtgärd av fel.

## Hur man gör det

För att skriva tester i Python används vanligtvis ett ramverk som heter "pytest". Detta ramverk tillhandahåller lättanvända funktioner för att definiera och köra tester. Nedan följer ett exempel på hur man skapar och kör en enkel testfunktion i pytest:

```Python
def add(x, y):
    return x + y

def test_add():
    assert add(2, 3) == 5
```

I detta exempel definieras en funktion för att lägga till två tal och en testfunktion som verifierar att resultatet är korrekt. Det "assert" uttalandet i testfunktionen kontrollerar om det förväntade resultatet är lika med det faktiska resultatet. Om det är sant passerar testet, annars misslyckas det. För att köra testerna, kan du använda följande kommando i terminalen:

```Python
pytest -v
```

Resultatet kommer att visa hur många tester som passerades och eventuellt också vilka som misslyckades.

## Djupdykning

När det kommer till att skriva tester, finns det flera olika typer av tester som kan användas för att säkerställa kodens kvalitet. En typ är enhetstester som testar enskilda funktioner eller moduler. En annan typ är integreringstester som testar interaktionen mellan flera komponenter i systemet.

Det är också viktigt att tänka på vilka scenarier och gränsvärden som bör testas för att upptäcka potentiella problem i koden. Det är också en bra idé att kontinuerligt skriva och köra tester under utvecklingsprocessen för att undvika att samla på sig en stor mängd testfall i slutet.

## Se även

 - [Dokumentation för pytest](https://docs.pytest.org/en/stable/index.html)
 - [En guide till enhetstestning i Python](https://semaphoreci.com/community/tutorials/testing-python-applications-with-pytest)