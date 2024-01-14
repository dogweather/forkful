---
title:    "Python: Unire le stringhe"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

In programmazione, spesso ci troviamo nella necessità di unire due o più stringhe per creare un unico risultato. Questa operazione è conosciuta come concatenazione di stringhe ed è fondamentale per svolgere numerose attività, come la creazione di messaggi personalizzati, la formattazione di testi e la costruzione di URL.

## Come Fare

Per concatenare due o più stringhe in Python, ci sono diversi modi, ma uno dei più semplici è utilizzare l'operatore "+".

```Python
stringa1 = "Ciao"
stringa2 = "mondo!"
concatenato = stringa1 + " " + stringa2
print(concatenato)
```

L'output verrà visualizzato come "Ciao mondo!" poiché l'operatore "+" unisce le due stringhe tra loro, separandole con uno spazio in questo caso. Si possono concatenare anche più di due stringhe, semplicemente aggiungendole una dopo l'altra.

Un altro modo per concatenare stringhe è utilizzare il metodo "format()". In questo caso, si utilizza una stringa come template, indicando con le parentesi graffe dove inserire le stringhe da concatenare.

```Python
stringa1 = "Ciao"
stringa2 = "mondo!"
concatenato = "{} {}".format(stringa1, stringa2)
print(concatenato)
```

L'output sarà lo stesso di prima, ovvero "Ciao mondo!".

## Approfondimento

Per comprendere meglio come funziona la concatenazione di stringhe in Python, è importante sapere che le stringhe sono considerate un tipo di dato immutabile. Ciò significa che, una volta assegnato un valore a una stringa, non è possibile modificarlo. Quando si utilizza l'operatore "+", il risultato sarà una nuova stringa e le stringhe originali non subiranno alcuna alterazione.

Inoltre, è importante tenere a mente che è possibile concatenare solo tipi di dati tra loro, quindi non si possono concatenare stringhe con numeri o altri tipi di dati.

## Vedi Anche

Per ulteriori informazioni sulla concatenazione di stringhe in Python, puoi consultare questi link:

- [Documentazione ufficiale di Python su stringhe](https://docs.python.org/3/library/string.html)
- [Tutorial su stringhe in Python](https://realpython.com/python-strings/)
- [Approfondimento su stringhe immutabili e modificabili in Python](https://www.programiz.com/python-programming/immutable-string)

Grazie per aver letto questo articolo e speriamo ti sia stato utile per comprendere questo importante concetto della programmazione in Python.