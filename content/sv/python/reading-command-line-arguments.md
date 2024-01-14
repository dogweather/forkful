---
title:    "Python: Att läsa kommandoradsargument"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att läsa inmatningskommandon från kommandotolken är en viktig del av Python-programmering. Genom att förstå hur man kan läsa och använda dessa argument kan du enkelt skapa interaktiva och dynamiska program. Det är också ett användbart verktyg för att utveckla skript och automatisera uppgifter.

## Hur man gör

Det första du behöver göra är att importera sys-modulen i ditt Python-skript:

```Python
import sys
```

Nästa steg är att lagra inmatningsargumenten i en variabel för enkel åtkomst. Detta görs genom att använda sys.argv-listan, där varje element är ett argument:

```Python
args = sys.argv
```

Nu kan vi enkelt få ut och använda varje argument genom att ange dess index i listan. Till exempel, om vi vill använda det första argumentet, skulle vi använda:

```Python
args[0]
```

Du kan också använda en for-loop för att gå igenom alla argument och utföra olika åtgärder baserat på deras värden. Se till att konvertera argumenten till lämpliga datatyper om det behövs.

```Python
for arg in args:
    # do something with the argument
```

## Djupdykning

Sys.argv-listan innehåller alltid minst ett argument, vilket är sökvägen till det aktuella Python-skriptet. Om du till exempel kör skriptet genom att skriva "python script.py" i kommandotolken, kommer args[0] att vara "script.py". Det andra argumentet som följer kommer att vara args[1] och så vidare.

Du kan också använda flaggor för att specificera olika parametrar till ditt skript. För att läsa in flaggor i sys.argv-listan, behöver du använda modulen argparse.

```Python
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--flag", help="beskrivning av flagga", type=int)
args = parser.parse_args()
```

Nu kan du använda args.flag för att få värdet av flaggan som skickas in i kommandotolken. Till exempel, om du kör skriptet med flaggan "--flag 5", kommer args.flag att ha värdet 5.

## Se även

- Dokumentation för sys-modulen: https://docs.python.org/3/library/sys.html
- Dokumentation för argparse-modulen: https://docs.python.org/3/library/argparse.html