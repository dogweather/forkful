---
title:                "Python: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Spesso quando programmiamo in Python, ci troviamo a lavorare con stringhe di testo diverse e dobbiamo svolgere delle operazioni su di esse. Una di queste operazioni comuni è l'estrazione di sottostringhe, ovvero una piccola parte di una stringa più grande. Anche se potrebbe sembrare una cosa semplice, l'estrazione delle sottostringhe può essere molto utile in diverse situazioni.

## Come Farlo

Per estrarre una sottostringa in Python, dobbiamo utilizzare il metodo ```.slice()```. Questo metodo accetta due parametri, il primo è l'indice iniziale della sottostringa e il secondo è l'indice finale. Vediamo un esempio di come utilizzare questo metodo per estrarre la sottostringa "mondo" dalla stringa "Ciao mondo!".

```Python
stringa = "Ciao mondo!"
sottostringa = stringa[5:10]

print(sottostringa)
```

Output: mondo

Nell'esempio sopra, abbiamo utilizzato gli indici 5 e 10 per indicare rispettivamente l'inizio e la fine della sottostringa "mondo". Possiamo anche utilizzare numeri negativi per gli indici, in modo che possiamo indicare la sottostringa dall'ultima lettera della stringa. Ad esempio, se vogliamo estrarre la sottostringa "mondo" dalla fine della stringa sopra, possiamo scrivere:

```Python
stringa = "Ciao mondo!"
sottostringa = stringa[-5:-1]

print(sottostringa)
```

Output: mondo

Oltre all'utilizzo di indici per estrarre sottostringhe, possiamo anche utilizzare il metodo ```.split()``` per dividere una stringa in una lista di sottostringhe utilizzando un delimitatore specificato. Ad esempio, se vogliamo dividere la stringa "Ciao mondo!" in due sottostringhe utilizzando lo spazio come delimitatore, possiamo scrivere:

```Python
stringa = "Ciao mondo!"
sottostringhe = stringa.split(" ")

print(sottostringhe)
```

Output: ["Ciao", "mondo!"]

## Approfondimento

Oltre ai metodi sopra descritti, esistono anche altre funzioni utili per l'estrazione di sottostringhe in Python. Una di queste è il metodo ```.find()```, che ci permette di trovare la posizione di una sottostringa all'interno di una stringa. Vediamo un esempio di utilizzo:

```Python
stringa = "Ciao mondo!"
posizione = stringa.find("mondo")

print(posizione)
```

Output: 5

Inoltre, possiamo utilizzare anche le espressioni regolari con il modulo ```re``` per trovare estrarre sottostringhe in base a un modello specificato.

## Vedi Anche

- Documentazione ufficiale di Python sull'estrazione di sottostringhe: https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str
- Tutorial su come utilizzare le espressioni regolari in Python: https://realpython.com/regex-python/
- Un'utile guida su come utilizzare gli indici per manipolare stringhe in Python: https://www.digitalocean.com/community/tutorials/how-to-index-and-slice-strings-in-python-3-it