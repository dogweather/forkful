---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:00.025006-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali significa creare valori imprevedibili tramite un computer. Programmatori lo fanno per tutto ciò che richiede elementi di casualità, come giochi, simulazioni o sicurezza informatica.

## How to:
Per generare un numero casuale in Bash, si può usare `$RANDOM` o comandi come `shuf` e `awk`.

```Bash
# Usando $RANDOM per ottenere un numero casuale tra 0 e 32767
echo $RANDOM

# Per ottenere un numero in un range specifico, ad esempio da 1 a 100
echo $((1 + RANDOM % 100))

# Usare shuf per generare un numero casuale tra 1 e 100
echo $(shuf -i 1-100 -n 1)

# Usare awk per generare un numero casuale tra 1 e 100
echo | awk 'BEGIN{srand(); print int(rand()*(100-1+1))+1 }'
```
Ecco un esempio d'uscita per ciascun comando:

```Bash
16253
57
84
29
```

## Deep Dive
La casualità in informatica è sempre stata una sfida perché i computer sono essenzialmente macchine deterministiche. `$RANDOM` è un generatore di numeri pseudo-casuali (PRNG) integrato in Bash. Tuttavia, per applicazioni che richiedono alta sicurezza, come la crittografia, è consigliato usare metodi più sofisticati come `/dev/random` o `/dev/urandom`, che si basano su eventi esterni per aumentare l'aleatorietà.

Altri comandi come `shuf` e `awk` servono a scopi più generali, ma possono essere utilizzati per generare numeri casuali. `shuf` mescola linee di input, e `awk` è un potente linguaggio di scripting in sé, che tra le altre cose può compiere operazioni matematiche e gestire la casualità.

Da notare che i numeri generati negli esempi sopra non sono adatti per la crittografia. Per la generazione di numeri casuali crittograficamente sicuri in Bash, si dovrebbero usare le interfacce ai sistemi operativi specifici, come `/dev/random` o `/dev/urandom`.

## See Also
Per comprendere meglio come Bash gestisce i numeri casuali:

- Manuale Bash su `$RANDOM`: https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#index-RANDOM
- Guida sugli strumenti `shuf`: https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html
- Tutorial di `awk`: https://www.gnu.org/software/gawk/manual/gawk.html

Per approfondire gli aspetti di sicurezza e i PRNG crittografici:

- https://unix.stackexchange.com/questions/324209/when-to-use-dev-random-vs-dev-urandom
- https://www.2uo.de/myths-about-urandom/
