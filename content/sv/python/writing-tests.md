---
title:                "Skriva tester"
html_title:           "Python: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av utvecklingsprocessen för att säkerställa att din kod fungerar som den ska. Det hjälper även till att upptäcka fel och förhindrar potentiella buggar i framtiden.

## Så här gör du

För att skriva tester i Python, används det inbyggda testramverket 'unittest'. Det börjar med att importera unittest biblioteket och de funktioner eller klasser som ska testas.

```Python
import unittest

def add(num1, num2):
    return num1 + num2

class TestCalc(unittest.TestCase):
    
    def test_add(self):
        self.assertEqual(add(2,3), 5)

if __name__ == '__main__':
    unittest.main()
```

I detta exempel skapas en enkel funktion som adderar två tal och sedan testas det med hjälp av 'unittest.TestCase' klassen. AssertEqual-funktionen kontrollerar om resultatet är det förväntade och om så är fallet så passerar testet. Om resultatet inte stämmer så misslyckas testet och en felmeddelande visas.

## Djupdykning

Vid skrivande av tester är det viktigt att täcka alla möjliga fall, både för korrekt och inkorrekt indata. Detta hjälper till att säkerställa att koden hanterar alla scenarier ordentligt och inte orsakar oväntade fel.

En annan viktig aspekt är att testen bör vara isolerade, vilket betyder att varje test ska köra oberoende av varandra. Detta förhindrar eventuella beroenden mellan olika tester och gör det lättare att felsöka om ett test fallerar.

Det finns också möjlighet att använda externa bibliotek för att hjälpa till med skrivandet av tester, som till exempel 'pytest' och 'nose'. Dessa erbjuder fler funktioner för att underlätta skrivandet av tester och öka testtäckningen för din kod.

## Se även

- [En guide till enhetstester i Python](https://realpython.com/python-testing/)
- [Dokumentation för unittest](https://docs.python.org/3/library/unittest.html)
- [Dokumentation för pytest](https://docs.pytest.org/en/latest/)
- [Dokumentation för nose](https://nose.readthedocs.io/en/latest/)