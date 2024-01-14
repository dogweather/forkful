---
title:    "Fish Shell: Utilizzando le espressioni regolari"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché Utilizzare le Espressioni Regolari nel Fish Shell

Le espressioni regolari sono una potente strumento utilizzato per riconoscere e manipolare testi secondo uno schema specifico. Nel Fish Shell, possono essere utilizzate per automazioni di compiti ripetitivi, ricerca di file e molte altre funzioni utili. Continua a leggere per scoprire come utilizzarle nel tuo codice.

## Come Utilizzare le Espressioni Regolari nel Fish Shell

Per utilizzare le espressioni regolari nel Fish Shell, è necessario utilizzare il comando "string match". Ad esempio, se vogliamo cercare la parola "pesce" all'interno di una stringa, possiamo utilizzare il seguente codice:

```
Fish Shell stringmatch 'pesce' 'Mi piace il pesce'
```

Questo comando restituirà "1" se la parola "pesce" è presente nella stringa e "0" se non è presente. Possiamo anche utilizzare espressioni regolari più complesse, come nel seguente esempio che cerca una parola con una "h" alla fine:

```
Fish Shell string match '*h' 'Pesche'
```

In questo caso, il risultato sarà sempre "1" poiché la parola "Pesche" termina con una "h". Possiamo anche utilizzare gli operator con le espressioni regolari, come nel seguente esempio che cerca una stringa che inizia con "p" e termina con "h":

```
Fish Shell string match 'p*h' 'Pesce'
```

Questo comando restituirà "1" poiché la stringa "Pesce" rispetta la regola specificata.

## Approfondimenti sull'utilizzo delle Espressioni Regolari

Oltre agli esempi sopra riportati, esistono molte altre funzioni e operatori che possono essere utilizzati con le espressioni regolari nel Fish Shell. Ad esempio, è possibile utilizzare l'operatore "not" per cercare una stringa che non corrisponde a una determinata espressione regolare.

Per ulteriori informazioni su come utilizzare le espressioni regolari nel Fish Shell, consulta la documentazione ufficiale su [Fish shell manuale](https://fishshell.com/docs/current/cmds/string.html#string-match) o il [Fish Shell Cookbook](https://fishshell.com/docs/current/cookbook.html#Regexes). Questi risorse ti aiuteranno a sfruttare appieno questa potente funzionalità del Fish Shell.

## Vedi anche

- [How to Use Regular Expressions in Bash](https://www.linode.com/docs/development/bash/use-regexes-in-bash/)
- [Using Regular Expressions in Python](https://realpython.com/regex-python/)
- [An Introduction to Regular Expressions](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285)