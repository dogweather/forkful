---
title:    "Python: Trova la lunghezza di una stringa"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

Spesso, quando si lavora con stringhe di testo in Python, può essere utile conoscere la lunghezza di una determinata stringa. Ad esempio, potresti avere bisogno di validare l'input dell'utente o di limitare la lunghezza di una stringa in un database. In questo post scoprirai come trovare la lunghezza di una stringa in maniera semplice ed efficace.

## Come fare

Per trovare la lunghezza di una stringa in Python, puoi utilizzare il metodo `len()`, che restituisce il numero di caratteri presenti nella stringa. Ad esempio, se hai una stringa `"Ciao mondo"`, puoi utilizzare il seguente codice per trovarne la lunghezza:

```Python
stringa = "Ciao mondo"
lunghezza = len(stringa)

print(lunghezza)
```

L'output di questo codice sarà `11`, poiché la stringa è composta da 11 caratteri. Per verificare che ciò sia effettivamente corretto, puoi contare manualmente i caratteri presenti nella stringa.

## Approfondimento

Detto questo, è importante notare che il metodo `len()` conta anche gli spazi vuoti e i caratteri speciali come parte della lunghezza della stringa. Inoltre, questo metodo può essere utilizzato anche per determinare la lunghezza di altre tipologie di dati, come le liste e le tuple.

Se vuoi ottenere una lunghezza più precisa, esiste un altro metodo chiamato `str.count()`, che conta il numero di occorrenze di una determinata sottostringa all'interno di una stringa più grande. Puoi anche specificare un intervallo di caratteri all'interno dei quali cercare. Ad esempio:

```Python
stringa = "Ciao mondo"
occorrenze = stringa.count('o', 0, 5)

print(occorrenze)
```

L'output sarà `2`, poiché stiamo contando solo le occorrenze della lettera "o" nelle prime 5 posizioni della stringa.

## Vedi anche

- [Documentazione di Python su `len()`](https://docs.python.org/3/library/functions.html#len)
- [Documentazione di Python su `str.count()`](https://docs.python.org/3/library/stdtypes.html#str.count)