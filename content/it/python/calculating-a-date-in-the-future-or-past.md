---
title:                "Python: Calcolare una data nel futuro o nel passato"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Perché

Calcolare una data nel futuro o nel passato può essere utile per programmatori che necessitano di effettuare calcoli di date in modo preciso e automatizzato, senza dover fare i calcoli manualmente.

# Come Fare

Puoi usare il modulo datetime di Python per calcolare date nel futuro o nel passato. Ecco un esempio di codice che calcola 10 giorni nel futuro a partire dalla data odierna:

```python
import datetime

# calcola la data odierna
data_odierna = datetime.date.today()

# aggiunge 10 giorni alla data odierna
data_futura = data_odierna + datetime.timedelta(days=10)

# stampa la data futura nel formato giorno-mese-anno
print(data_futura.strftime("%d-%m-%Y"))
```

Output: 10-10-2021

E se invece volessimo calcolare 3 mesi nel passato? Ecco il codice per farlo:

```python
import datetime

# calcola la data odierna
data_odierna = datetime.date.today()

# sottrae 3 mesi dalla data odierna
data_passata = data_odierna - datetime.timedelta(weeks=12)

# stampa la data passata nel formato giorno-mese-anno
print(data_passata.strftime("%d-%m-%Y"))
```

Output: 10-06-2021

# Deep Dive

Il modulo datetime di Python offre diverse funzionalità per calcolare date nel futuro o nel passato. Ad esempio, è possibile utilizzare il metodo `replace()` per modificare singole componenti della data, come il giorno, il mese o l'anno. Inoltre, il modulo fornisce anche una classe `timedelta` che consente di effettuare aggiunte o sottrazioni di giorni, settimane o mesi.

Un'altra funzionalità interessante è il calcolo del giorno della settimana a partire da una data specifica. Questo può essere fatto utilizzando la classe `weekday()` che restituisce un numero intero corrispondente al giorno della settimana (0 per lunedì, 1 per martedì, ecc.).

# Vedi Anche

- Documentazione ufficiale del modulo datetime di Python: https://docs.python.org/3/library/datetime.html
- Tutorial su come gestire le date in Python: https://realpython.com/python-datetime/
- Esempi di calcolo di date nel futuro o nel passato con Python: https://www.programiz.com/python-programming/datetime