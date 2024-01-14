---
title:    "Python: Convertire una data in una stringa"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa è una delle operazioni più comuni nel mondo della programmazione. È utile per visualizzare le date in un formato specifico o per elaborare input utente. In questo post, impareremo come convertire una data in una stringa nel linguaggio di programmazione Python.

## Come fare

Per convertire una data in una stringa in Python, possiamo utilizzare il metodo `strftime()` della libreria `datetime`. Ecco un esempio di codice che illustra come utilizzarlo:

```Python 
import datetime

data = datetime.datetime(2021, 8, 25)

stringa = data.strftime("%d/%m/%Y")

print(stringa)
```

L'output di questo codice sarà: `25/08/2021`. Nell'esempio sopra, abbiamo creato un oggetto `datetime` con la data 25 agosto 2021. Quindi, utilizzando il metodo `strftime()` abbiamo specificato il formato in cui vogliamo visualizzare la data. In questo caso, `%d` rappresenta il giorno, `%m` rappresenta il mese e `%Y` rappresenta l'anno. Ci sono molti altri formati disponibili, puoi consultarli nella [documentazione ufficiale di Python](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes).

Se invece vogliamo convertire la data in una stringa in un formato leggibile, possiamo utilizzare il metodo `str()` invece di `strftime()`. Ecco un esempio:

```Python 
import datetime

data = datetime.datetime(2021, 8, 25)

stringa = str(data)

print(stringa)
```

L'output di questo codice sarà: `2021-08-25 00:00:00`. In questo caso, la data viene convertita in una stringa nel formato predefinito di Python.

## Approfondimento

Ci sono alcune cose da tenere a mente quando si lavora con la conversione di date in stringhe in Python. In primo luogo, è importante specificare il formato corretto nella chiamata a `strftime()` per ottenere il risultato desiderato. Inoltre, è possibile combinare vari caratteri di formato per creare una stringa di data personalizzata. Ad esempio, possiamo utilizzare `%a` per indicare il nome abbreviato del giorno della settimana o `%b` per il nome abbreviato del mese.

Inoltre, se vogliamo convertire una stringa in un oggetto `datetime`, possiamo utilizzare il metodo `strptime()` della libreria `datetime`. Ecco un esempio:

```Python 
import datetime

stringa = "25-08-2021"

data = datetime.datetime.strptime(stringa, "%d-%m-%Y")

print(data)
```

L'output di questo codice sarà: `2021-08-25 00:00:00`. In questo caso, abbiamo specificato il formato desiderato della stringa utilizzando `%d`, `%m` e `%Y` e il metodo `strptime()` ha convertito correttamente la stringa in un oggetto `datetime`.

## Vedi anche

- [Documentazione ufficiale di Python sulla libreria datetime](https://docs.python.org/3/library/datetime.html)
- [Articolo su come lavorare con date e ore in Python](https://realpython.com/python-datetime/)
- [Video tutorial su come convertire date in stringhe in Python](https://www.youtube.com/watch?v=eirjjyP2qcQ)