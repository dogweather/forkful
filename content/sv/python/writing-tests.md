---
title:                "Att skriva tester"
html_title:           "Python: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester är en process där programmerare skapar kod för att testa sin egen kod. Detta används för att säkerställa att koden fungerar som den ska och för att minska risken för buggar och fel.

## Hur gör man:
För att skriva tester i Python används vanligtvis en modul som heter "unittest". Detta tillåter programmerare att skapa olika tester för sina funktioner och klasser. Här är ett enkelt exempel:

```Python
# importera unittest-modulen
import unittest

# skapa en enkel funktion för att addera två tal
def add(a, b):
  return a + b

# skapa ett testfall för att se om funktionen add() fungerar som det ska
class TestAdd(unittest.TestCase):

  # en testmetod för att testa en enkel addition
  def test_simple_add(self):
    result = add(2, 3)
    self.assertEqual(result, 5) # jämför resultatet med det förväntade värdet

# kör testerna
unittest.main()
```

Expected output:
```Python
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

## Deep Dive:
Att skriva tester är en viktig del av processen för att utveckla programvara. Genom att testa kod regelbundet minskar risken för potentiella buggar och fel, vilket sparar tid och resurser i det långa loppet. Alternativ till "unittest" är bland annat "pytest" och "doctest", men "unittest" är det mest populära alternativet för Python-programmerare. Det är också viktigt att notera att tester ska vara en del av den kontinuerliga utvecklingsprocessen, inte bara något som görs i slutet.

## Se även:
- [Python enhetstester (documentation)](https://docs.python.org/3/library/unittest.html)
- [pytest](https://docs.pytest.org/en/latest/)
- [doctest](https://docs.python.org/3/library/doctest.html)