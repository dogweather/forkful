---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generazione di numeri casuali in Bash

## Cosa e Perché?

Generare numeri casuali in programmazione è un modo per ottenere risultati variabili, rendendo le applicazioni più dinamiche ed imprevedibili. I programmatori lo fanno per simulazioni, giochi, crittografia, e tester i loro codici in scenari vari.

## Come fare:

Generare un numero casuale in bash è semplice grazie alla variabile speciale `RANDOM`. 

```Bash
echo $RANDOM
```
L'output sarà un numero casuale tra 0 e 32767.

Se vuoi un numero casuale dentro un certo intervallo, usa il modulo `%`. Per esempio, per un numero tra 0 e 99:

```Bash
echo $((RANDOM % 100))
```

## Un tuffo più profondo:

Il generatore di numeri casuali è un elemento chiave nel sistema di linguaggio shell Bash. Il suo precursore, la shell Bourne, non disponeva di questa funzionalità, che è stata tuttavia inclusa in Bash sin dalla sua prima apparizione nel 1989.

Una possibile alternativa all'uso di `RANDOM` è il programma `shuf` per generare numeri casuali in un intervallo specificato.

Grazie alla portabilità di Bash, `RANDOM` è implementato su molte piattaforme. Tuttavia, non è adatto per la crittografia in quanto non produce sequenze veramente casuali.

## Vedi Anche:

- `man bash`: Per ulteriori dettagli su come Bash genera numeri casuali. Puoi accedere a questa guida attraverso la riga di comando.
- `man shuf`: Per saperne di più sull'alternativa del programma `shuf`.
- [Bash Guide](https://tldp.org/LDP/abs/html/randomvar.html): Una guida per saperne di più sulle variabili casuali bash.