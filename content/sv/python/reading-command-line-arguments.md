---
title:                "Läsning av kommandoradsargument"
html_title:           "Python: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
 Om du är nyfiken på Python och vill lära dig mer om hur man hanterar kommandoradsargument, då har du kommit till rätt ställe! Att lära sig om kommandoradsargument kan hjälpa dig att skapa mer dynamiska och användarvänliga Python-program, så det är en färdighet som är väl värd att lägga tid på.

## Hur man
För att börja läsa kommandoradsargument i Python, behöver du först importera `sys`modulen. Sedan kan du använda `sys.argv` för att åtkomstkommandoradsargument som en lista. Låt oss titta på ett exempel:
```Python
import sys

print("Namn på det körbara programmet: ", sys.argv[0])
print("Kommandoradsargument: ", sys.argv[1:])
```
Kör detta program med några argument, till exempel "python program.py arg1 arg2 arg3", så kommer du att se följande utmatning:
```
Namn på det körbara programmet: program.py
Kommandoradsargument: ['arg1', 'arg2', 'arg3']
```

## Djupdykning
Förutom att bara läsa kommandoradsargument, kan du också använda `argparse`-modulen för att skapa mer avancerade funktioner, som att definiera argumenttyper, beskrivningar och krav. Detta är speciellt användbart för att skapa mer användarvänliga program som kan hantera felaktiga eller saknade argument. Här är ett exempel på hur man kan använda `argparse`:
```Python
import argparse

parser = argparse.ArgumentParser(description="En enkel kalkylator")
parser.add_argument('--operation', type=str, help="Välj räknesätt: add, subtract, multiply, divide", required=True)
parser.add_argument('--numbers', nargs='+', type=float, help="Skriv in siffror som du vill räkna med", required=True)
args = parser.parse_args()

if args.operation == 'add':
    result = sum(args.numbers)
elif args.operation == 'subtract':
    result = args.numbers[0] - sum(args.numbers[1:])
elif args.operation == 'multiply':
    result = 1
    for num in args.numbers:
        result *= num
elif args.operation == 'divide':
    result = args.numbers[0] / sum(args.numbers[1:])
else:
    print("Räknesättet är inte giltigt.")

print("Resultat: {}".format(result))
```
Om du kör detta program med argumentet `--operation multiply --numbers 2 3 4`, så kommer du att få utmatningen `Resultat: 24`.

## Se också
- [Python's sys modul](https://docs.python.org/3/library/sys.html)
- [Argparse dokumentation](https://docs.python.org/3/library/argparse.html)