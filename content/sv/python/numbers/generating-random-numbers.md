---
date: 2024-01-27 20:35:11.764879-07:00
description: "Hur man g\xF6r: Python tillhandah\xE5ller modulen `random` som hj\xE4\
  lper till att generera slumpm\xE4ssiga tal f\xF6r olika anv\xE4ndningsomr\xE5den.\
  \ S\xE5 h\xE4r kommer du ig\xE5ng:\u2026"
lastmod: '2024-03-13T22:44:37.479108-06:00'
model: gpt-4-0125-preview
summary: "Python tillhandah\xE5ller modulen `random` som hj\xE4lper till att generera\
  \ slumpm\xE4ssiga tal f\xF6r olika anv\xE4ndningsomr\xE5den."
title: Generera slumptal
weight: 12
---

## Hur man gör:
Python tillhandahåller modulen `random` som hjälper till att generera slumpmässiga tal för olika användningsområden. Så här kommer du igång:

1. **Importera modulen**
    ```Python
    import random
    ```

2. **Generera ett slumpmässigt heltal**
    Mellan två valfria tal.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Exempel på utdata: `7`

3. **Generera ett flyttal**
    Mellan 0 och 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Exempel på utdata: `0.436432634653`

    Om du behöver ett flyttal inom ett annat intervall, multiplicera:
    ```Python
    random_float_range = random.random() * 5  # 0 till 5
    print(random_float_range)
    ```
    Exempel på utdata: `3.182093745`

4. **Välj ett slumpmässigt element från en lista**
    ```Python
    greetings = ['Hej', 'Hallå', 'Tjena', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Exempel på utdata: `Hola`

5. **Blanda en lista**
    Perfekt för kortspel eller andra tillämpningar som behöver slumpmässig ordning.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Exempel på utdata: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Fördjupning
Modulen `random` i Python använder en pseudoslumpgenerator (PRNG), specifikt Mersenne Twister-algoritmen, som är bra för allmänna applikationer men inte lämplig för kryptografiska ändamål på grund av dess förutsägbarhet om tillräckligt många utdata observeras. Modulen `secrets`, som introducerades i Python 3.6, erbjuder ett bättre alternativ för att generera kryptografiskt säkra slumpmässiga tal, särskilt användbart i säkerhetskänsliga applikationer. Till exempel generering av en säker, slumpmässig token för en länk för återställning av lösenord:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Historiskt sett har det varit en utmaning inom databehandling att generera verkligen slumpmässiga tal, med tidiga metoder som förlitade sig på fysiska fenomen eller manuellt inmatade frön. Utvecklingen och antagandet av algoritmer som Mersenne Twister (som används som standard i Pythons modul `random` åtminstone fram till min senaste kunskapsuppdatering i 2023) markerade ett betydande framsteg. Dock har den pågående sökningen efter mer säkra och effektiva algoritmer lett till inkluderingen av modulen `secrets` för kryptografirelaterade uppgifter. Denna utveckling speglar den växande vikten av säkerhet inom mjukvaruutveckling och behovet av mer robust slumpmässighet i applikationer som sträcker sig från kryptering till säker token-generering.
