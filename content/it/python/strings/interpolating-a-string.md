---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:24:04.242459-07:00
description: "L'interpolazione di stringhe \xE8 il metodo di incorporamento di espressioni\
  \ all'interno di letterali di stringa. I programmatori lo usano per inserire\u2026"
lastmod: '2024-03-13T22:44:42.987506-06:00'
model: gpt-4-0125-preview
summary: "L'interpolazione di stringhe \xE8 il metodo di incorporamento di espressioni\
  \ all'interno di letterali di stringa. I programmatori lo usano per inserire\u2026"
title: Interpolazione di una stringa
weight: 8
---

## Cosa & Perché?
L'interpolazione di stringhe è il metodo di incorporamento di espressioni all'interno di letterali di stringa. I programmatori lo usano per inserire dinamicamente valori nelle stringhe, rendendo il codice più leggibile e pulito rispetto alla tradizionale concatenazione di stringhe.

## Come fare:
In Python 3.6 e versioni successive, puoi interpolare le stringhe utilizzando le f-string. Ecco come:

```Python
nome = 'Alice'
età = 30
saluto = f"Ciao, {nome}. Hai {età} anni."

print(saluto)
```

Output:
```
Ciao, Alice. Hai 30 anni.
```

Puoi anche usare espressioni all'interno delle parentesi graffe:

```Python
a = 5
b = 10
info = f"Cinque più dieci fa {a + b}, non {2 * (a + b)}."

print(info)
```

Output:
```
Cinque più dieci fa 15, non 30.
```

## Approfondimento
Prima di Python 3.6, `.format()` era il metodo usato per l'interpolazione di stringhe:

```Python
nome = 'Bob'
età = 25
saluto = "Ciao, {}. Hai {} anni.".format(nome, età)

print(saluto)
```

Il vecchio metodo Python (versioni < 2.6) usava l'operatore `%` per l'interpolazione, il quale è meno intuitivo e può diventare complicato con molteplici variabili:

```Python
nome = 'Carol'
età = 35
saluto = "Ciao, %s. Hai %d anni." % (nome, età)

print(saluto)
```

Oltre a una sintassi più pulita, le f-string sono più veloci perché vengono valutate a runtime e poi convertite direttamente in un'operazione di formattazione di stringhe efficiente. Il metodo `.format()` e l'operatore `%` implicano più passaggi e sono più lenti.

## Vedi Anche
- [PEP 498 – Interpolazione Letterale di Stringhe](https://www.python.org/dev/peps/pep-0498/) per la documentazione ufficiale sulle f-string.
- [Le f-string di Python](https://realpython.com/python-f-strings/) di Real Python per un tutorial sull'uso delle f-string.
- [Il Metodo .format()](https://docs.python.org/3/library/stdtypes.html#str.format) nella documentazione di Python per comprendere il vecchio metodo di formattazione di stringhe `.format()`.
