---
title:                "Python: Convertire una data in una stringa."
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è un'operazione comune nella programmazione, soprattutto quando si lavora con informazioni relative al tempo come scadenze o orari. È importante essere in grado di manipolare le date in modo da adattarle alle proprie esigenze e renderle facilmente leggibili per gli utenti.

## Come fare

Per convertire una data in una stringa in Python, è possibile utilizzare il metodo `strftime()` che è disponibile nel modulo `datetime`. Vediamo un esempio:

```Python
import datetime

# Creiamo una data
data = datetime.date(2021, 5, 23)

# Utilizziamo il metodo strftime() per convertire la data in una stringa
data_stringa = data.strftime("%d/%m/%Y")

# Stampiamo la stringa
print(data_stringa)
```

L'output di questo codice sarà `23/05/2021`. Vediamo cosa significa il parametro `"%d/%m/%Y"` utilizzato nel metodo `strftime()`:

- `%d` rappresenta il giorno del mese in formato numerico a due cifre (es. 23)
- `%m` rappresenta il mese in formato numerico a due cifre (es. 05)
- `%Y` rappresenta l'anno in formato numerico a quattro cifre (es. 2021)

È possibile utilizzare altri parametri per formattare la stringa della data in base alle proprie esigenze. Ad esempio, `%b` rappresenta l'abbreviazione del mese (es. Mag), `%A` rappresenta il nome del giorno della settimana (es. Domenica), `%y` rappresenta l'anno in formato numerico a due cifre (es. 21).

## Approfondimento

La rappresentazione delle date in Python fa utilizzo del modulo `datetime` che permette di manipolare e formattare le date in vari modi. La classe `date` fornisce metodi per accedere alle informazioni relative al giorno, al mese e all'anno di una determinata data. Inoltre, nel modulo `datetime` sono disponibili anche le classi `time` e `datetime` per gestire le informazioni relative all'ora. È utile conoscere questi moduli per poter creare applicazioni che richiedono una gestione accurata del tempo.

## Vedi anche

- [Documentazione ufficiale di Python su datetime](https://docs.python.org/3/library/datetime.html)
- [Tutorial su come manipolare le date in Python](https://www.programiz.com/python-programming/datetime)
- [Articolo su come gestire le informazioni temporali in Python](https://realpython.com/python-datetime/)