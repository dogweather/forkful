---
title:                "Python: Ottenerne la data corrente."
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si chiedono perché dovrebbero prendere la data corrente durante la scrittura del codice. In realtà, c'è una serie di motivi per cui potresti aver bisogno di ottenere la data corrente. Ad esempio, potresti voler effettuare una determinata operazione solo in un giorno specifico o potresti dover registrare la data e l'ora di creazione di un file o di un record nel tuo database. Fortunatamente, con Python, ottenere la data corrente è molto semplice e veloce!

## Come

Per ottenere la data corrente in Python, possiamo utilizzare il modulo `datetime`. Inizia importando il modulo:

```python
import datetime
```

Nota: Se utilizzi la versione 2 di Python, è necessario importare `datetime` dal modulo `datetime`.

Una volta importato il modulo, possiamo utilizzare il metodo `now()` per ottenere la data corrente:

```python
now = datetime.datetime.now()
```

Questo restituirà un oggetto `datetime` che rappresenta la data e l'ora correnti. Possiamo quindi utilizzare il metodo `strftime()` per formattare la data in modo da ottenere solo le parti che ci interessano. Ad esempio, se vogliamo ottenere la data nel formato `DD/MM/YYYY`, possiamo utilizzare il seguente codice:

```python
print(now.strftime("%d/%m/%Y"))
```

Ecco un esempio di output: `12/05/2021`

Puoi anche ottenere solo la data o solo l'ora utilizzando rispettivamente i metodi `date()` e `time()`:

```python
print(now.date()) # restituisce la data
print(now.time()) # restituisce l'ora
```

## Deep Dive

Oltre ai modi semplici di ottenere la data corrente, il modulo `datetime` offre anche una serie di metodi per manipolarla e lavorare con le date in generale. Ad esempio, puoi creare oggetti `datetime` specificando una data e un'ora specifiche:

```python
specific_date = datetime.datetime(2021, 5, 5, 12, 30, 15) # anno, mese, giorno, ora, minuto, secondo
print(specific_date)
```

Ecco l'output: `2021-05-05 12:30:15`

Puoi anche aggiungere o sottrarre giorni, ore, minuti o secondi a una data utilizzando i metodi `timedelta()` e `replace()`:

```python
new_date = specific_date + datetime.timedelta(days=10) # aggiunge 10 giorni alla data specificata
print(new_date.replace(hour=10, minute=30)) # sostituisce l'ora e i minuti della data specificata
```

Ecco l'output: `2021-05-15 10:30:00`

Questi sono solo alcuni esempi delle funzionalità avanzate offerte dal modulo `datetime`.

## Vedi anche

- [Documentazione ufficiale di Python sul modulo `datetime`](https://docs.python.org/3/library/datetime.html)
- [Tutorial su come utilizzare il modulo `datetime` in Python](https://realpython.com/python-datetime/)
- [Stack Overflow: Ottenere la data corrente in Python](https://stackoverflow.com/questions/415511/how-to-get-current-time-in-python)