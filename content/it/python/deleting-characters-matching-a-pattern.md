---
title:                "Eliminare i caratteri corrispondenti a un modello"
html_title:           "Python: Eliminare i caratteri corrispondenti a un modello"
simple_title:         "Eliminare i caratteri corrispondenti a un modello"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché eliminare i caratteri corrispondenti a un modello?

Ci sono diverse ragioni per cui potresti voler eliminare i caratteri corrispondenti a un determinato modello all'interno del tuo codice Python. Potresti aver bisogno di ripulire i tuoi dati da informazioni indesiderate o di trasformare una stringa in un formato diverso che non includa alcune parti. In generale, l'eliminazione dei caratteri corrispondenti a un modello può essere utile per rendere i tuoi dati più puliti e gestibili.

## Come fare

Per eliminare i caratteri corrispondenti a un modello in Python, puoi utilizzare la funzione "re.sub()" del modulo "re". Questa funzione accetta tre argomenti: il modello che vuoi cercare, la stringa in cui vuoi sostituire il modello e la stringa di origine. Ad esempio, se vuoi eliminare tutte le vocali dalla stringa "ciao mondo", puoi utilizzare il seguente codice:

```python
import re

stringa = "ciao mondo"
nuova_stringa = re.sub(r'[aeiou]', '', stringa)
print(nuova_stringa)
```

L'output di questo codice sarà "c mnd", poiché tutte le vocali sono state eliminate dalla stringa di origine. È importante notare che il modello deve essere scritto tra parentesi quadre e che l'argomento "r" prima del modello indica che si tratta di una stringa grezza (raw string), evitando così la necessità di utilizzare caratteri di escape.

Se vuoi eliminare più di un carattere alla volta, puoi utilizzare gli operatori "+" e "*". "+" indica uno o più occorrenze del carattere, mentre "*" indica eventuali occorrenze del carattere. Ad esempio, continuando con l'esempio precedente, se vuoi eliminare tutte le vocali e tutte le lettere minuscole, puoi utilizzare il seguente codice:

```python
nuova_stringa = re.sub(r'[aeiou]+', '', stringa)
nuova_stringa = re.sub(r'[a-z]*', '', nuova_stringa)
print(nuova_stringa)
```

L'output di questo codice sarà semplicemente uno spazio vuoto, poiché tutti i caratteri sono stati eliminati dalla stringa di origine.

## Deep Dive

La funzione "re.sub()" è solo una delle opzioni disponibili per eliminare i caratteri corrispondenti a un modello in Python. Puoi anche utilizzare la funzione "re.findall()" per trovare le corrispondenze del modello nella stringa di origine e poi eliminare questi caratteri utilizzando i metodi delle stringhe come "replace()" o "translate()".

Inoltre, puoi utilizzare espressioni regolari più complesse all'interno del modello, per esempio, per eliminare solo le vocali maiuscole o solo le consonanti che seguono una vocale. C'è una grande flessibilità nella scrittura dei modelli e puoi adattarli alle tue esigenze specifiche.

## Vedi anche

- [Documentazione ufficiale di Python](https://docs.python.org/3/library/re.html)
- [Tutorial dettagliato sulle espressioni regolari in Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)
- [Articolo su come utilizzare espressioni regolari per la pulizia dei dati](https://towardsdatascience.com/cleaning-data-using-regular-expressions-python-regex-explained-6281ddd3c721)