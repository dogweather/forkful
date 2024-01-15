---
title:                "Estrazione di sottostringhe"
html_title:           "Fish Shell: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore alla ricerca di un modo semplice e efficiente per estrarre sottocategorie da una stringa, allora dovresti considerare l'utilizzo della shell Fish. Grazie alle sue potenti funzionalità, puoi facilmente estrarre substringhe utilizzando il suo codice semplice e intuitivo.

## Come Fare

Per estrarre una sottostringa da una stringa in Fish, utilizzare il comando `string sub`. Ad esempio, se hai una stringa "Ciao mondo!" e vuoi estrarre solo la parola "mondo", puoi farlo con il seguente codice:

```
Fish Shell

string sub "Ciao mondo!" - 5
```

La sintassi è semplice: inserisci la parola "string" seguita dall'argomento "sub" e la stringa da cui vuoi estrarre la sottostringa. Inoltre, puoi specificare la posizione iniziale della sottostringa utilizzando un segno meno prima del numero della posizione desiderata.

Il risultato del codice sopra riportato dovrebbe essere il seguente output:

```
Fish Shell

mondo!
```

Puoi anche utilizzare il comando `strindex` per trovare la posizione di un carattere o di una sottostringa all'interno di una stringa. Ad esempio:

```
Fish Shell

string strindex "Ciao mondo!" "mondo!"
```

Il risultato sarà il seguente:

```
Fish Shell

5
```

## Approfondimento

È possibile specificare ulteriori argomenti per ottenere risultati più precisi. Ad esempio, puoi utilizzare l'opzione `start` per indicare la posizione di partenza della sottostringa, e l'opzione `end` per indicare la posizione di fine. Inoltre, puoi utilizzare l'opzione `length` per specificare la lunghezza desiderata della sottostringa.

```
Fish Shell

string sub "Ciao mondo!" --start 5 --length 5
```

Il risultato del codice sopra riportato sarà:

```
Fish Shell

mondo
```

Sperimenta con questi comandi e le loro opzioni per ottenere i risultati desiderati.

## Vedi Anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guida rapida per iniziare con Fish Shell](https://dev.to/mauroker0x/improve-your-terminal-game-with-fish-shell-2538)