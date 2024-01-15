---
title:                "Convertire una stringa in minuscolo"
html_title:           "Python: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui si potrebbe voler convertire una stringa in minuscolo in Python. Ad esempio, potrebbe essere necessario confrontare due stringhe senza considerare le differenze tra maiuscole e minuscole, o forse si desidera rendere uniforme il formato di una lista di parole.

## Come fare

Per convertire una stringa in minuscolo in Python, è possibile utilizzare il metodo `.lower()` sulla stringa stessa.

```python
stringa = "Ciao a TuTti"
print(stringa.lower())
```

```
ciao a tutti
```

Si noti che il metodo `.lower()` restituisce una nuova stringa convertita in minuscolo, lasciando la stringa originale inalterata. Possiamo anche assegnare il risultato di `.lower()` a una nuova variabile o direttamente stamparlo.

```python
parola = "cIao"
nuova_parola = parola.lower()
print(nuova_parola)
```

```
ciao
```

Inoltre, è possibile utilizzare il metodo `.casefold()` per una conversione ancor più accurata, in quanto tiene conto anche delle differenze regionali nella lingua.

```python
stringa = "Ollè"
print(stringa.casefold())
```

```
ollé
```

## Approfondimento

È importante notare che la conversione in minuscolo non è sempre così semplice come sembra. Ad esempio, alcune lingue hanno lettere che possono essere rappresentate in più modi, come la lettera "i" in turco che può essere scritta con un punto sopra di essa, detto "i con i turchi".

Per gestire queste sfide, Python offre il modulo `unicodedata`, che permette di normalizzare le stringhe contenenti caratteri unicode. È possibile utilizzare la funzione `normalize()` per convertire la stringa in una delle forme normalizzate.

```python
import unicodedata

parola = "i con i turchi"
print(unicodedata.normalize('NFKC', parola.lower()))
```

```
i con i turchi
```

Inoltre, è importante considerare che la conversione in minuscolo può comportare problemi di performance, in quanto richiede l'elaborazione di ogni singolo carattere della stringa. Se si sta lavorando con testi molto lunghi, potrebbe essere opportuno utilizzare l'operatore `lower()` solo su una parte della stringa, se necessario.

## Vedi anche

Per ulteriori informazioni sulle stringhe in Python, si consiglia di leggere la documentazione ufficiale:

- [Documentazione ufficiale delle stringhe in Python](https://docs.python.org/3/library/string.html)
- [Documentazione ufficiale del modulo unicodedata](https://docs.python.org/3/library/unicodedata.html)