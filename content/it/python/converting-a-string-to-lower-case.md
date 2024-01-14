---
title:    "Python: Convertire una stringa in minuscolo"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è un'operazione fondamentale quando si lavora con dati di testo. Ciò consente di uniformare le stringhe e semplifica le operazioni di ricerca e confronto all'interno del codice.

## Come fare

Per convertire una stringa in minuscolo in Python, è possibile utilizzare il metodo `lower()`. Ecco un esempio di come farlo:

```python
testo = "QUESTA STRINGA SARA' TUTTA IN MINUSCOLO"
print(testo.lower())
```

Questo codice produrrà l'output: `questa stringa sara' tutta in minuscolo`.

## Approfondimento

Il metodo `lower()` converte tutti i caratteri alfabetici all'interno della stringa in minuscolo. Ciò significa che le lettere accentate saranno anche convertite, quindi attenzione quando si lavora con testi in lingue diverse dall'inglese. Se si desidera convertire solo la prima lettera di una stringa in minuscolo, si può utilizzare il metodo `capitalize()`. Inoltre, è possibile utilizzare il metodo `casefold()` per una conversione più accurata delle lettere accentate.

## Vedi anche

- Documentazione ufficiale di Python sul metodo `lower()`: https://docs.python.org/3/library/stdtypes.html#str.lower
- Tutorial su come manipolare stringhe in Python: https://realpython.com/python-strings/
- Progetti pratici per applicare le operazioni di manipolazione delle stringhe: https://www.w3resource.com/python-exercises/string/