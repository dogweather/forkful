---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:21.595202-07:00
description: "Het genereren van willekeurige getallen houdt in dat er getallen worden\
  \ gecre\xEBerd die redelijkerwijs niet beter te voorspellen zijn dan bij toeval,\
  \ wat\u2026"
lastmod: '2024-03-13T22:44:50.372356-06:00'
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen houdt in dat er getallen worden\
  \ gecre\xEBerd die redelijkerwijs niet beter te voorspellen zijn dan bij toeval,\
  \ wat\u2026"
title: Willekeurige getallen genereren
weight: 12
---

## Wat & Waarom?

Het genereren van willekeurige getallen houdt in dat er getallen worden gecreëerd die redelijkerwijs niet beter te voorspellen zijn dan bij toeval, wat essentieel is voor het ontwikkelen van simulaties, games en beveiligingsalgoritmen. Programmeurs doen dit om onvoorspelbaarheid te introduceren of realistische fenomenen in hun applicaties te simuleren.

## Hoe te:

Python biedt de `random` module die helpt bij het genereren van willekeurige getallen voor verschillende doeleinden. Hier is hoe je kunt beginnen:

1. **Het importeren van de module**
    ```Python
    import random
    ```

2. **Een willekeurig geheel getal genereren**
    Tussen twee getallen.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Voorbeelduitvoer: `7`

3. **Een willekeurige zwevende-kommagetel genereren**
    Tussen 0 en 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Voorbeelduitvoer: `0.436432634653`

    Als je een zwevende-kommagetel in een ander bereik nodig hebt, vermenigvuldig dan:
    ```Python
    random_float_range = random.random() * 5  # 0 tot 5
    print(random_float_range)
    ```
    Voorbeelduitvoer: `3.182093745`

4. **Een willekeurig element uit een lijst kiezen**
    ```Python
    greetings = ['Hallo', 'Hoi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Voorbeelduitvoer: `Hola`

5. **Een lijst schudden**
    Perfect voor kaartspellen of elke toepassing die behoefte heeft aan willekeurige volgorde.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Voorbeelduitvoer: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Diepere duik

De `random` module in Python gebruikt een pseudowillekeurige nummergenerator (PRNG), specifiek het Mersenne Twister-algoritme, dat goed is voor algemene toepassingen maar niet geschikt voor cryptografische doeleinden vanwege de voorspelbaarheid als voldoende uitvoer is waargenomen. De `secrets` module, geïntroduceerd in Python 3.6, biedt een beter alternatief voor het genereren van cryptografisch sterke willekeurige getallen, met name nuttig in beveiligingsgevoelige toepassingen. Bijvoorbeeld, het genereren van een veilige, willekeurige token voor een wachtwoordresetlink:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Historisch gezien was het genereren van werkelijk willekeurige getallen een uitdaging in de informatica, met vroege methoden die afhankelijk waren van fysieke fenomenen of handmatig ingevoerde zaden. De ontwikkeling en adoptie van algoritmen zoals Mersenne Twister (standaard gebruikt in Python's `random` module tot ten minste mijn laatste kennisupdate in 2023) markeerde een significante vooruitgang. Echter, de voortdurende zoektocht naar meer veilige en efficiënte algoritmen heeft geleid tot de opname van de `secrets` module voor cryptografiegerelateerde taken. Deze evolutie weerspiegelt het groeiende belang van beveiliging in softwareontwikkeling en de behoefte aan robuustere willekeurigheid in toepassingen variërend van encryptie tot het genereren van veilige tokens.
