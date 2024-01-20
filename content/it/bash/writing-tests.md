---
title:                "Scrivere test"
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Scrivere test in programmazione significa creare script che verificano se pezzi di codice funzionano come previsto. Si fa per garantire la qualità del software, per trovare errori, e per prevenire future regressioni.

## Come fare:
Ecco un esempio semplice di come scrivere e eseguire un test in Bash:

```Bash
#!/bin/bash

function somma {
  echo $(($1 + $2))
}

# Test
risultato=$(somma 2 3)
atteso=5

if [ "$risultato" -eq "$atteso" ]; then
  echo "Test passato: 2 + 3 = $atteso"
else
  echo "Test fallito: 2 + 3 NON è $atteso, invece è $risultato"
fi
```

Output:
```
Test passato: 2 + 3 = 5
```

## Approfondimento:
Nel mondo della programmazione, scrivere test è ormai uno standard dagli anni '70. Esistono framework di testing specifici, come shUnit2 per Bash, mentre alternative moderne includono strumenti come Bats o frameworks per altri linguaggi di scripting. Per essere efficace, il testing deve coprire varie situazioni di errore e di successo (test cases), ed essere integrato nello sviluppo continuo.

## Vedi Anche:
- shUnit2: https://github.com/kward/shunit2
- Bats: https://github.com/bats-core/bats-core
- Bash Automated Testing System: https://en.wikipedia.org/wiki/Bash_Automated_Testing_System