---
title:                "Capitalizzare una stringa"
html_title:           "Bash: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Capitalizzare una stringa significa trasformare tutte le sue lettere in maiuscole. I programmatori lo fanno per vari motivi come l'uniformità dei dati, eseguire confronti insensibili alle maiuscole, e rendere il testo più leggibile.

## Come fare:

Ecco un esempio di come capitalizzare una stringa in Bash:

```Bash
stringa="ciao mondo"
stringa_maiuscola="${stringa^^}"
echo $stringa_maiuscola
```

Se esegui questo script, l'output sarà:

```Bash
CIAO MONDO
```

## Approfondimento

Historicamente, la capacità di cambiare il caso delle stringhe era limitata in Bash. È stata introdotta nella versione 4.0, rendendo più facile lavorare con le stringhe.
Ci sono alternative per capitalizzare una stringa se stai usando una versione più vecchia di Bash, come l'uso di `tr`:

```Bash
echo "ciao mondo" | tr '[:lower:]' '[:upper:]'
```

Questo trasforma tutte le lettere minuscole in maiuscole.

In termini di implementazione, `"${stringa^^}"` usa il parametro dell'espansione del shell per cambiare il caso. Questa è una funzione del Bash stesso, e non richiede programmi esterni.

## Vedere Anche

Per saperne di più sulla capitalizzazione delle stringhe e su Bash in generale, consulta le seguenti risorse:

1. Bash Parameter Expansion: http://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
2. Bash Scripting Guide: https://www.tldp.org/LDP/abs/html/
3. String Manipulation in Bash: https://linuxconfig.org/bash-scripting-tutorial-for-beginners