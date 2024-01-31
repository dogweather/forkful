---
title:                "Generering av tilfeldige tall"
date:                  2024-01-27T20:34:56.575367-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generering av tilfeldige tall"

category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Generering av tilfeldige tall innebærer å skape tall som ikke kan forutsies med rimelighet bedre enn ved sjansen, noe som er essensielt for utviklingen av simuleringer, spill og sikkerhetsalgoritmer. Programmører gjør dette for å innføre uforutsigbarhet eller simulere fenomener fra den virkelige verden i applikasjonene sine.

## Hvordan:

Python tilbyr `random`-modulen som hjelper i generering av tilfeldige tall for ulike bruksområder. Her er hvordan du kommer i gang:

1. **Importere modulen**
    ```Python
    import random
    ```

2. **Generere et tilfeldig heltall**
    Mellom to hvilke som helst tall.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Eksempel på utdata: `7`

3. **Generere et flyttall**
    Mellom 0 og 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Eksempel på utdata: `0.436432634653`

    Hvis du trenger et flyttall i et annet spenn, multipliser:
    ```Python
    random_float_range = random.random() * 5  # 0 til 5
    print(random_float_range)
    ```
    Eksempel på utdata: `3.182093745`

4. **Velge et tilfeldig element fra en liste**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Eksempel på utdata: `Hola`

5. **Stokke en liste**
    Perfekt for kortspill eller enhver applikasjon som trenger å randomisere rekkefølgen.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Eksempel på utdata: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Fordypning

`Random`-modulen i Python bruker en pseudotilfeldig tallgenerator (PRNG), spesifikt Mersenne Twister-algoritmen, som er god for generelle applikasjoner, men ikke egnet for kryptografiske formål på grunn av forutsigbarheten hvis nok utdata observeres. `Secrets`-modulen, introdusert i Python 3.6, tilbyr et bedre alternativ for generering av kryptografisk sterke tilfeldige tall, spesielt nyttig i sikkerhetssensitive applikasjoner. For eksempel, generering av en sikker, tilfeldig token for en tilbakestilling av passordlenke:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Historisk har generering av tilfeldige tall som virkelig er tilfeldige, vært en utfordring i databehandling, med tidlige metoder som stoler på fysiske fenomener eller manuelt inntastede frø. Utviklingen og adopsjonen av algoritmer som Mersenne Twister (brukt som standard i Pythons `random`-modul til minst min siste kunnskapsoppdatering i 2023) markerte betydelig fremgang. Imidlertid har den pågående søken etter mer sikre og effektive algoritmer ført til inkluderingen av `secrets`-modulen for kryptografirelaterte oppgaver. Denne utviklingen reflekterer den voksende betydningen av sikkerhet i programvareutvikling og behovet for mer robust tilfeldighet i applikasjoner som spenner fra kryptering til generering av sikre token.
