---
title:                "Estrazione di sottostringhe"
html_title:           "Python: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Forse ti è capitato di dover lavorare con stringhe di testo e hai avuto bisogno di estrarre una porzione specifica di queste stringhe. Estrarre substrings è una delle operazioni più comuni nel mondo della programmazione, e sapere come farlo può rendere il tuo codice più efficiente e flessibile.

## Come fare

Per estrarre un substrings in Python, puoi utilizzare il metodo `substring()` su una stringa o utilizzare la sintassi `[start:end]` per estrarre una porzione specifica della stringa.

Ecco un esempio di come utilizzare il metodo `substring()` su una stringa:

```Python 
testo = "Benvenuti al mio articolo su substrings!"
print(testo.substring(11, 17)) # output: articolo
```

Ecco invece un esempio di come utilizzare la sintassi `[start:end]`:

```Python
testo = "Ciao a tutti!"
print(testo[6:]) # output: tutti!
```

Come puoi vedere, nella sintassi `[start:end]` è possibile omettere il valore di `start` per estrarre dall'inizio della stringa, oppure omettere il valore di `end` per estrarre fino alla fine della stringa.

## Approfondimento

Ci sono diverse altre funzioni e metodi che puoi utilizzare per estrarre substrings in Python, come ad esempio `split()`, `rstrip()` e `replace()`. Inoltre, puoi utilizzare delle espressioni regolari per ottenere ancora più flessibilità nell'estrazione di substrings.

Se vuoi saperne di più, puoi consultare la documentazione ufficiale di Python su stringhe e substrings.

## Vedi anche

- [Documentazione ufficiale di Python su stringhe e substrings](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str) 
- [Python string methods](https://www.w3schools.com/python/python_ref_string.asp) 
- [Regular Expressions in Python](https://docs.python.org/3/library/re.html#module-re)