---
title:    "Python: Concatenare stringhe"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione comune e utile che consente di combinare più stringhe in un'unica stringa. Questo può essere utile per creare messaggi personalizzati, elaborare dati o semplicemente per unire insieme informazioni.

## Come fare

Per concatenare stringhe in Python, possiamo utilizzare l'operatore "+" o il metodo ".join()". Ecco un esempio di entrambi i metodi:

```Python
# Utilizzando l'operatore "+"
stringa1 = "Ciao"
stringa2 = "mondo"
stringa3 = stringa1 + stringa2
print(stringa3)
# Output: Ciaomondo

# Utilizzando il metodo ".join()"
stringa4 = " ".join([stringa1, stringa2])
print(stringa4)
# Output: Ciao mondo
```

Come si può vedere dagli esempi, l'uso di "+" è semplice e diretto, mentre il metodo ".join()" richiede una lista di stringhe da unire e un separatore (in questo caso uno spazio).

## Approfondimento

La concatenazione di stringhe è possibile grazie al fatto che in Python le stringhe sono immutabili, il che significa che non possono essere modificate direttamente. Invece, quando viene effettuata una concatenazione, viene creata una nuova stringa contenente i caratteri delle stringhe precedenti.

È importante notare che la concatenazione di grandi quantità di stringhe può essere inefficiente, poiché ogni volta che viene eseguita una concatenazione viene creato un nuovo oggetto stringa. In questi casi, può essere più conveniente utilizzare il metodo ".join()" o altri metodi più efficienti come l'uso di liste e then conversione finale in stringa.

## Vedi anche

- [Documentazione ufficiale di Python sulla concatenazione di stringhe](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Articolo su Real Python sulle operazioni con le stringhe in Python](https://realpython.com/python-strings/)
- [Tutorial su DataCamp sull'uso del metodo ".join()" per la concatenazione di stringhe](https://www.datacamp.com/community/tutorials/python-strings-share#join)