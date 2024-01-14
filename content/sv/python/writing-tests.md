---
title:    "Python: Skriva tester"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att utveckla pålitlig och hållbar kod. Genom att skriva tester för din kod kan du säkerställa att den fungerar korrekt och minska risken för buggar och felaktig funktionalitet i ditt program.

## Hur du gör

Att skriva tester i Python är enkelt och följer ett standardiserat format. Du kan använda inbyggda testbibliotek som pytest eller unittest, eller till och med skapa dina egna anpassade testramar. Nedan följer ett exempel på hur du kan skriva och köra tester för en funktion som adderar två tal:

```

# Importera testbiblioteket unittest
import unittest

# Definiera funktionen som du vill testa
def add(a, b):
    return a + b

# Skapa en testklass som ärver från unittest.TestCase
class TestAddFunction(unittest.TestCase):

    # Skapa ett testfall för att säkerställa att addition av två positiva tal fungerar korrekt
    def test_positive_numbers(self):
        result = add(2, 3) # Anropa funktionen med två tal som ska adderas
        self.assertEqual(result, 5) # Använd assertEqual för att jämföra resultatet med förväntat värde

# Kör testerna genom att köra unittest-modulen
if __name__ == '__main__':
    unittest.main()

```

Utmatningen från dessa tester bör se ut så här:

```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

Det här betyder att testet lyckades och inga felaktigheter upptäcktes. Om testet misslyckades skulle det ge en tydlig varning om vilket test som gick fel och vad som förväntades.

## Djupdykning

Att skriva tester är inte bara ett sätt att kontrollera att din kod fungerar korrekt, det kan också hjälpa dig att förbättra ditt kodskrivande. Genom att tänka på hur din kod ska testas när du skriver den, tvingar du dig själv att skriva mer läsbar och modulär kod. Dessutom kan du alltid återkomma till dina tester när du gör förändringar i koden för att se till att inget har gått sönder.

Det finns många andra tekniker och konventioner för att skriva tester som du kan utforska för att bli en bättre programvarutillverkare. Att skriva tester är en ovärderlig färdighet som du bör omfamna för att förbättra din kodning.

## Se även

- [En introduktion till enhetstester i Python](https://realpython.com/python-testing/)
- [Implementering av enhetstester i ett Python-projekt](https://www.jetbrains.com/help/pycharm/testing-your-first-python-application.html)