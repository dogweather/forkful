---
title:                "Python: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är ett viktigt koncept inom programmering. Det kan användas för att skapa olika användarscenarion, skapa slumpmässiga spel eller för att testa kod.

## Hur man genererar slumpmässiga nummer i Python
Generera slumpmässiga heltal mellan 0 och 10:
```Python
import random
print(random.randint(0, 10))
```

Generera slumpmässiga decimaltal mellan 0 och 1:
```Python
import random
print(random.uniform(0, 1))
```

Generera slumpmässiga tecken från en lista:
```Python
import random
lista = ['a', 'b', 'c', 'd']
print(random.choice(lista))
```

Skapa en lista med slumpmässiga heltal:
```Python
import random
lista = random.sample(range(100), 10)
print(lista)
```

Output:
```
[47, 10, 73, 31, 82, 95, 56, 20, 38, 98]
```

## Djupdykning
Det finns flera olika sätt att generera slumpmässiga nummer i Python. Det vanligaste är att använda modulen "random" som innehåller olika metoder för att skapa slumpmässiga värden. Det finns också möjlighet att använda andra moduler som "numpy" och "scipy" för mer avancerade funktioner.

När man genererar slumpmässiga nummer är det viktigt att förstå att de egentligen inte är helt slumpmässiga utan genereras efter en algoritm. Det är därför viktigt att välja en lämplig metod beroende på vad syftet är med de slumpmässiga numren.

## Se även
- [Dokumentation för modulen "random" i Python](https://docs.python.org/3/library/random.html)
- [Tutorial för att generera slumpmässiga nummer i Python](https://medium.com/@meghamohan/everything-you-need-to-know-about-random-module-in-python-d7b738138b11)
- [Mer avancerade metoder för att generera slumpmässiga nummer med numpy](https://www.numpy.org/devdocs/reference/random/index.html)