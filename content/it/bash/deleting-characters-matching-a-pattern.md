---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

L'eliminazione di caratteri corrispondenti a un pattern è l'atto di rimuovere tutte le istanze di un modello specifico da una stringa. Lo facciamo per pulire i dati, standardizzare le stringhe, e molto altro.

## Come fare:

Supponiamo di voler rimuovere tutte le occorrenze di 'a' da una stringa. E' facile farlo con `tr -d`:

```Bash
echo "banana" | tr -d 'a'
# Output: bnn
```
Vogliamo rimuovere sia 'a' che 'b'? Non c'è problema:

```Bash
echo "banana" | tr -d 'ab'
# Output: nn
```
E se volessimo rimuovere una sequenza di caratteri, come 'an'? Per questo, useremmo `sed`:

```Bash
echo "banana" | sed 's/an//g'
# Output: baa
```

## Un Diving più Profondo

La censura dei caratteri è un concetto antico in informatica, risalente a prima che UNIX facesse il suo debutto negli anni '70. `tr` e `sed` sono entrambi degli strumenti di manipolazione di testo di vecchia generazione che hanno resistito alla prova del tempo.

Tuttavia, esistono alternative. Ad esempio, Perl offre un potente supporto per le espressioni regolari che supera di gran lunga quello di `tr` e `sed`. Potreste anche utilizzare un linguaggio di programmazione di livello superiore come Python o Ruby, che offrono metodi integrati per la gestione delle stringhe.

In termini di implementazione, `tr` e `sed` funzionano leggendo l'input riga per riga, sostituendo o eliminando i modelli di caratteri come richiesto. Prestare attenzione a questa caratteristica quando si lavora con file di grandi dimensioni, poiché potrebbe avere implicazioni sulla memoria.

## Approfondimenti

Per ulteriori informazioni sulla manipolazione delle stringhe in Bash, si possono consultare i seguenti link:

[a. Breve introduzione a SED](https://www.grymoire.com/Unix/Sed.html#uh-0)  
[b. Manuale di SED](http://sed.sourceforge.net/sed1line_it.html)  
[c. Corso online di Bash Scripting](https://www.codecademy.com/learn/learn-the-command-line/modules/bash-scripting)