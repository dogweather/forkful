---
date: 2024-01-27 20:35:17.062028-07:00
description: "La generazione di numeri casuali comporta la creazione di numeri che\
  \ non possono essere previsti in modo ragionevole meglio che per caso, il che \xE8\
  \u2026"
lastmod: 2024-02-19 22:05:02.104060
model: gpt-4-0125-preview
summary: "La generazione di numeri casuali comporta la creazione di numeri che non\
  \ possono essere previsti in modo ragionevole meglio che per caso, il che \xE8\u2026"
title: Generazione di numeri casuali
---

{{< edit_this_page >}}

## Cosa & Perché?

La generazione di numeri casuali comporta la creazione di numeri che non possono essere previsti in modo ragionevole meglio che per caso, il che è essenziale per lo sviluppo di simulazioni, giochi e algoritmi di sicurezza. I programmatori fanno ciò per introdurre imprevedibilità o simulare fenomeni del mondo reale nelle loro applicazioni.

## Come fare:

Python fornisce il modulo `random` che aiuta nella generazione di numeri casuali per vari usi. Ecco come iniziare:

1. **Importazione del modulo**
    ```Python
    import random
    ```

2. **Generazione di un Intero Casuale**
    Tra due numeri qualsiasi.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Output di esempio: `7`

3. **Generazione di un Float**
    Tra 0 e 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Output di esempio: `0.436432634653`

    Se hai bisogno di un float in un diverso intervallo, moltiplica:
    ```Python
    random_float_range = random.random() * 5  # Da 0 a 5
    print(random_float_range)
    ```
    Output di esempio: `3.182093745`

4. **Scelta di un Elemento Casuale da una Lista**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Output di esempio: `Hola`

5. **Mescolare una Lista**
    Perfetto per giochi di carte o qualsiasi applicazione che necessita di randomizzare l'ordine.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Output di esempio: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Approfondimento

Il modulo `random` in Python utilizza un generatore di numeri pseudo-casuali (PRNG), in particolare l'algoritmo Mersenne Twister, che è buono per applicazioni di uso generale ma non adatto per scopi crittografici a causa della sua prevedibilità se vengono osservati abbastanza output. Il modulo `secrets`, introdotto in Python 3.6, offre un'alternativa migliore per generare numeri casuali criptograficamente forti, particolarmente utile in applicazioni sensibili alla sicurezza. Ad esempio, generando un token casuale e sicuro per un link di reimpostazione della password:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Storicamente, generare numeri casuali che sono veramente casuali è stata una sfida nel campo dell'informatica, con i primi metodi che si affidavano a fenomeni fisici o semi inseriti manualmente. Lo sviluppo e l'adozione di algoritmi come Mersenne Twister (utilizzato di default nel modulo `random` di Python fino almeno al mio ultimo aggiornamento delle conoscenze nel 2023) hanno segnato un significativo progresso. Tuttavia, la continua ricerca di algoritmi più sicuri ed efficienti ha portato all'inclusione del modulo `secrets` per compiti legati alla crittografia. Questa evoluzione riflette l'importanza crescente della sicurezza nello sviluppo del software e la necessità di una casualità più robusta in applicazioni che vanno dalla crittografia alla generazione di token sicuri.
