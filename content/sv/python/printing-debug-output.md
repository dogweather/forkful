---
title:    "Python: Utmatning av felsökningsinformation"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är en spännande och ibland utmanande process. Och ibland, oavsett hur bra vi är på att planera och skriva vår kod, så stöter vi på problem. Det kan vara ett resultat som inte ser ut precis som vi förväntade oss, ett oväntat felmeddelande eller till och med en kod som helt enkelt inte fungerar som den ska. För att hjälpa oss att felsöka dessa problem är det viktigt att använda verktyg som kan hjälpa oss att förstå vad som händer i vår kod. Och det är där utskrift av debugdata blir värdefullt.

## Hur man gör

Det enklaste sättet att skriva ut debugdata i Python är att använda "print" -funktionen. Vi kan antingen skriva ut ett meddelande som vi vill se, eller så kan vi skriva ut värdena på våra variabler för att se om de innehåller rätt data.

```Python
# Skriv ut ett meddelande
print("Debugdata")

# Skriv ut värdet på en variabel
num = 10
print("Värdet på num är:", num)
```

Output:
```
Debugdata
Värdet på num är: 10
```

En annan användbar teknik är att använda "assert" -satser för att kontrollera våra antaganden om våra variabler och koden i allmänhet.

```Python
# Kontrollera att variabeln är av rätt typ
my_list = [1, 2, 3]
assert isinstance(my_list, list)

# Kontrollera att ett villkor är sant
my_bool = True
assert my_bool is True
```

## Djupdykning

Att skriva ut debugdata kan hjälpa oss att identifiera fel och problem i vår kod, men det är också viktigt att vara försiktig med hur mycket data vi skriver ut. Om vi skriver ut för mycket data kan det bli svårt att hitta det som är relevant och det kan också påverka prestandan i vår kod.

En annan viktig aspekt är att veta var man ska skriva ut debugdata. Detta kan bero på vilken del av koden vi vill undersöka, vilken typ av data vi behöver eller hur ofta vi behöver informationen. Vi kan använda olika tekniker och verktyg för att skriva ut och samla in debugdata, som till exempel loggning och användning av debugger.

## Se även

- [Debugging i Python: Vad det är och hur man gör det](https://realpython.com/python-debugging/)
- [Writing Python Scripts](https://www.python.org/dev/peps/pep-0301/)
- [Python Debugging With the PDB Module](https://realpython.com/python-debugging-pdb/)
- [Python Logging Library](https://docs.python.org/3/library/logging.html)