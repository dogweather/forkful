---
title:                "Utskrift av felsökningsutdata"
html_title:           "Python: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Debuggning är en viktig del av programmering för att hitta och lösa fel i koden. Genom att skriva ut debuggutdata kan du få en bättre förståelse för din kod och snabbt hitta och åtgärda eventuella problem.

## Hur man gör
För att skriva ut debuggutdata i Python, använd print() funktionen. Här är ett enkelt exempel:

```Python
x = 5
y = 10
print("x = ", x)
print("y = ", y)
```
Output:
```Shell
x = 5
y = 10
```

För att lägga till mer information i din debuggutdata, kan du använda formatters som %s, %d, %f för att sätta in variabler i strängar. Till exempel:

```Python
name = "Anna"
age = 25
print("Hej, mitt namn är %s och jag är %d år gammal." %(name, age))
```
Output:
```Shell
Hej, mitt namn är Anna och jag är 25 år gammal.
```

Du kan också använda funktionen repr() för att skriva ut lite mer detaljerad information om variabler, som till exempel listor eller objekt. Till exempel:

```Python
list = [1,2,3]
print("Lista: " + repr(list))
```
Output:
```Shell
Lista: [1, 2, 3]
```

## Djupdykning
När du börjar använda print() funktionen för debugging, är det viktigt att vara medveten om att du inte vill lämna kvar debuggutdata i din slutliga kod. Detta kan leda till prestandaproblem eller orsaka oavsiktliga utskrifter till användaren.

För att undvika detta kan du använda en debuggflagga som kontrollerar om debuggutdata ska skrivas ut eller inte. Till exempel:

```Python
debug = True # Sätt till False när koden är klar för produktion
if debug:
    print("Debugginfo:")
    # Din debuggutdata här
```

En annan viktig aspekt att komma ihåg är att för större projekt kan det vara värt att överväga att använda ett dedikerat debuggsystem som ger dig mer omfattande information och kontroll över din debuggutdata.

## Se även
- [Python Documentations - print()](https://docs.python.org/3/library/functions.html#print)
- [RealPython - Python Debugging: Getting Started](https://realpython.com/python-debugging/)
- [Medium - Debugging in Python: 2020 Edition](https://medium.com/@jasonrigden/a-guide-to-python-debugging-2020-edition-e16e05dc0364)