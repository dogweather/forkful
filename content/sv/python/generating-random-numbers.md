---
title:    "Python: Generera slumpmässiga tal"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att kunna generera slumpmässiga nummer är en viktig del av programmering. Det kan användas för att skapa testdata, slumpvisa val i spelappar, och även för att förbättra säkerheten i lösenordsgenerering.

## Så här gör du

För att generera slumpmässiga nummer i Python, kan du använda funktionen `randint()` i inbyggda random-modulen. Detta låter dig välja ett slumpmässigt heltal mellan två givna tal.

```Python
import random

# Generera ett slumpmässigt heltal mellan 1 och 10
random_number = random.randint(1, 10)

print(random_number)
# Output: Slumpmässigt heltal mellan 1 och 10

```

Om du vill använda decimaltal istället kan du använda den inbyggda `random()` funktionen. Detta ger dig ett slumpmässigt flyttal mellan 0 och 1.

```Python
import random

# Generera ett slumpmässigt decimaltal mellan 0 och 1
random_decimal = random.random()

print(random_decimal)
# Output: Slumpmässigt flyttal mellan 0 och 1

```

Om du vill ange ett intervall för dina slumpmässiga decimaltal, kan du använda `uniform()` funktionen. Detta låter dig välja ett slumpmässigt flyttal mellan två givna tal.

```Python
import random

# Generera ett slumpmässigt decimaltal mellan 5 och 10
random_decimal = random.uniform(5, 10)

print(random_decimal)
# Output: Slumpmässigt flyttal mellan 5 och 10

```

## Djupdykning

Att generera slumpmässiga nummer kan även vara användbart för att simulera slumpmässiga händelser, som i datavetenskapliga algoritmer eller simuleringar.

Det finns även många användbara inbyggda metoder i Python som kan hjälpa dig att hantera slumpmässiga nummer. Till exempel kan `choice()` funktionen välja ett slumpmässigt element från en lista, `shuffle()` funktionen slumpmässigt ordna om en lista, och `sample()` funktionen välja ett slumpmässigt urval från en lista.

## Se även

- Dokumentation för den inbyggda random-modulen i Python: https://docs.python.org/3/library/random.html
- En guide för att generera slumpmässiga tal i Python: https://realpython.com/python-random/
- En tutorial för att simulera slumpmässiga händelser i Python: https://www.datacamp.com/community/tutorials/simulating-random-processes-python