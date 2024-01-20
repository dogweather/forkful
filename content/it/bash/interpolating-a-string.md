---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

L'interpolazione delle stringhe in Bash permette di inserire il valore di una variabile all'interno di una stringa. Questa operazione è utilizzata dai programmatori per costruire stringhe dinamiche in modo semplice e leggibile.

## Come si fa:

```Bash
# Definisci una variabile
nome="Giovanni"

# Utilizza la variabile nella stringa
echo "Ciao, $nome" # Stampa: "Ciao, Giovanni"
```

```Bash 
# Puoi anche utilizzare le parentesi per chiarire dove finisce il nome della variabile
cognome="Rossi"
echo "Ciao, ${nome}${cognome}" # Stampa: "Ciao, GiovanniRossi"
```

## Approfondimenti

L'interpolazione delle stringhe è un concetto che esiste in molti linguaggi di programmazione, e in Bash è presente sin dalla versione 2.0, pubblicata nel 1997. Un'alternativa all'interpolazione delle stringhe è la concatenazione, ma l'interpolazione è generalmente preferita per la sua leggibilità.

Per utilizzare l'interpolazione delle stringhe in Bash, fai attenzione a utilizzare le doppie virgolette `"` e non le singole `'`, in quanto quest'ultime impediranno l'interpolazione.

## Vedi Anche

- [Differenza tra interpolazione delle stringhe e concatenazione](https://stackoverflow.com/questions/2909633/what-is-string-interpolation-in-php)
- [Documentazione ufficiale Bash su variabili](http://www.gnu.org/software/bash/manual/bash.html#Shell-Variables)