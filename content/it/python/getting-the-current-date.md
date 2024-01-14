---
title:    "Python: Ottenere la data corrente"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

Scrivere codice per ottenere la data corrente può sembrare banale, ma in realtà è un'attività molto utile per qualsiasi programmatore Python. Conoscere la data o l'ora corrente è fondamentale per una varietà di applicazioni, come programmi di prenotazione, sistemi di cronologia e report giornalieri. In questo articolo impareremo come ottenere la data corrente utilizzando Python e scopriremo i vari modi in cui questa informazione può essere utile.

## Come fare

Per ottenere la data corrente in Python, ci sono diversi modi. Uno dei più semplici è utilizzare il modulo `datetime`, che facilita la gestione di date e orari in Python.

```Python
import datetime

# Ottieni la data odierna
oggi = datetime.date.today()
print(oggi)

# Ottieni l'anno corrente
anno = oggi.year
print(anno)

# Ottieni il mese corrente
mese = oggi.month
print(mese)

# Ottieni il giorno corrente
giorno = oggi.day
print(giorno)

# Ottieni la data completa con il formato personalizzato
data = oggi.strftime("%d/%m/%Y")
print(data)
```

L'output di questo codice sarà:

```
2021-10-03
2021
10
03
03/10/2021
```

Possiamo anche utilizzare il modulo `time` per ottenere l'orario corrente, utilizzando la funzione `time()`.

```Python
import time

# Ottieni l'orario corrente in secondi dal 1 gennaio 1970
seconds = time.time()
print(seconds)

# Otteni l'orario corrente in una stringa leggibile
ora = time.ctime()
print(ora)

# Usa il formato personalizzato
ora_format = time.strftime("%H:%M:%S")
print(ora_format)
```

L'output di questo codice sarà:

```
1633231254.1846983
Sun Oct  3 12:47:34 2021
12:47:34
```

## Più in profondità

Ora che sappiamo come ottenere la data e l'orario correnti, possiamo approfondire un po' di più. Come probabilmente avrai notato, le funzioni utilizzate nei codici di esempio sopra restituiscono sempre i valori attuali, ovvero aggiornati all'istante in cui il codice viene eseguito. Ma cosa succede se vogliamo ottenere una data o un orario specifici? Per fare ciò, dobbiamo utilizzare il modulo `datetime` in combinazione con l'oggetto `timedelta`, che ci permette di aggiungere o sottrarre un certo numero di giorni, ore, minuti o secondi dalla data corrente.

```Python
import datetime

# Aggiungi 5 giorni alla data corrente
future_date = datetime.date.today() + datetime.timedelta(days=5)
print(future_date)

# Sottrai 3 ore dall'orario corrente
future_time = datetime.datetime.now() - datetime.timedelta(hours=3)
print(future_time)
```

L'output di questo codice sarà qualcosa di simile a:

```
2021-10-08
2021-10-03 09:47:34.240331
```

Inoltre, Python offre anche altre funzioni utili per lavorare con le date, come ad esempio `weekday()` che restituisce il giorno della settimana corrispondente alla data fornita, o `isoweekday()` che restituisce il giorno della settimana come numero, anziché il nome.

## Vedi anche

- [Documentazione ufficiale di Python sul modulo datetime](https://docs.python.org/3/library/datetime.html)
- [Esempi di codice per lavorare con le date in Python](https://www.programiz.com/python-programming/datetime)
- [Tutorial di Real Python su come lavorare con le date e gli orari in Python](https://realpython.com/python-date-time/)