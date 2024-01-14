---
title:    "Python: Läsning av kommandoradsargument"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför

Att läsa kommandoradsargument kan vara en ovärderlig kunskap för Python-programmerare. Det ger dig möjlighet att interagera med ditt program på ett enkelt och effektivt sätt genom att ange specifika inställningar och parametrar vid körning. Detta kan spara tid och ge en smidigare användarupplevelse.

# Hur man gör det

Först och främst behöver du importera modulen "sys" som ger tillgång till systemspecifika funktioner och variabler. Sedan kan du använda sys.argv för att få en lista med alla argument som skickas med när programmet körs från kommandoraden. Här är ett exempel:

```Python
import sys

print("Antal argument:", len(sys.argv))
print("Argument:", sys.argv)
```

Om du kör detta program med argumentet "hello world", kommer det att se ut så här när du kör det från kommandoraden:

```Python
$ python program.py hello world
Antal argument: 3
Argument: ['program.py', 'hello', 'world']
```

För att få åtkomst till ett specifikt argument kan du använda dess position i listan. Till exempel, om du vill få ut värdet "hello", kan du använda sys.argv[1]. Kom ihåg att sys.argv[0] alltid kommer att vara värdet på själva programfilen.

# Djupdykning

Det finns några viktiga saker att notera när du läser kommandoradsargument i Python. Först och främst måste du alltid konvertera argumenten till lämpliga datatyper. Standardvärdena i sys.argv-listan är alltid strängar, så om du behöver ett heltal eller en flyttal måste du konvertera det med hjälp av int() eller float(). Du bör också hantera felaktig eller otillräcklig användarinput genom att använda try-except-satser.

En annan användbar teknik är att använda modulen "argparse" som ger dig möjlighet att definiera och hantera kommandoradsargument på ett mycket mer strukturerat sätt. Detta kan vara särskilt användbart för större projekt som har många olika argument och alternativ.

# Se även

- [The Python sys module documentation](https://docs.python.org/3/library/sys.html)
- [The Python argparse module documentation](https://docs.python.org/3/library/argparse.html)