---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:45.087984-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal innebär att skapa nummer som inte kan förutsägas, något som är essentiellt för spelutveckling, säkerhetssystem och vetenskapliga simuleringar. Programmerare behöver dessa tal för att testa algoritmer, skapa unika användarupplevelser, och säkerställa dataintegritet.

## Hur gör man:
För att generera ett slumpmässigt tal i Python använder vi modulen `random`. Här är några grundläggande exempel:

```Python
import random

# Slumpmässigt heltal mellan 1 och 10
rand_int = random.randint(1, 10)
print(rand_int)

# Slumpmässigt flyttal mellan 0 och 1
rand_float = random.random()
print(rand_float)

# Slumpmässigt val ur en lista
choices = ['äpple', 'banan', 'citron']
rand_choice = random.choice(choices)
print(rand_choice)
```

Det här ska generera något i stil med:

```
7
0.4356732132
banan
```

Kom ihåg att resultatet varierar varje gång du kör koden.

## Fördjupning:
Python's `random` modul använder Mersenne Twister som generator, en algoritm känd för hög kvalitet på slumpmässigheten. Den är bra till mycket, men inte lämpad för kryptografiskt säkra uppgifter. För detta finns `secrets` modulen.

Historiskt sätt har datorer haft svårt med att generera verkligt slumpmässiga tal. Tidiga metoder inkluderade användning av externa fysikaliska händelser. Numera använder vi algoritmer som Mersenne Twister och andra komplexa metoder.

Om man behöver något mer förutsägbart, till exempel vid testning, kan man sätta en "seed" så att sekvensen av slumpmässiga tal blir densamma varje gång.

```Python
random.seed(42)
# Nu kommer slumpmässiga tal att bli samma vid varje körning
```

## Se även:
- Python `random` dokumentation: https://docs.python.org/3/library/random.html
- `secrets` modul för säkra slumpmässiga tal: https://docs.python.org/3/library/secrets.html
- En diskussion om slumpmässighets algoritmer på Stack Overflow: https://stackoverflow.com/questions/22842289/generate-n-unique-random-numbers-within-a-range