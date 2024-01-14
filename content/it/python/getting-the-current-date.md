---
title:                "Python: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Saper ottenere la data corrente è un'abilità utile e fondamentale per tutti i programmatori Python. Anche se può sembrare una cosa semplice, la capacità di ottenere la data corrente è estremamente importante in situazioni in cui è necessario monitorare e registrare il tempo, come ad esempio nei sistemi di tracciamento delle attività o nell'elaborazione dei dati finanziari. In questo post scopriremo come ottenere la data corrente utilizzando Python.

## Come fare

Per ottenere la data corrente, utilizzeremo il modulo `datetime` di Python. Iniziamo importando il modulo nel nostro script:

```Python
import datetime
```

Per ottenere la data corrente, possiamo utilizzare la funzione `datetime.now()`, che restituirà un oggetto `datetime` con la data e l'orario correnti. Possiamo anche specificare un fuso orario come argomento, se necessario.

```Python
current_date = datetime.now()
print(current_date)
```

L'output dovrebbe essere qualcosa del genere:

```
2021-10-20 15:20:37.886834
```

Per accedere alla data e all'orario in modo più leggibile, possiamo utilizzare alcuni metodi delle classi `date` e `time` fornite dal modulo `datetime`.

Ad esempio, per ottenere solo la data nel formato "giorno/mese/anno" possiamo utilizzare il metodo `strftime` con il formato `%d/%m/%Y`:

```Python
print(current_date.strftime('%d/%m/%Y'))
```

L'output sarà:

```
20/10/2021
```

Possiamo anche accedere ai singoli elementi della data e dell'orario, come ad esempio il giorno, il mese o l'anno, utilizzando i metodi `day`, `month` e `year`:

```Python
print(current_date.day)
print(current_date.month)
print(current_date.year)
```

L'output dovrebbe essere rispettivamente:

```
20
10
2021
```

Oltre a ottenere la data corrente, possiamo utilizzare il modulo `datetime` anche per eseguire operazioni come il calcolo della differenza tra due date o la creazione di una data specifica.

## Deep Dive

Il modulo `datetime` offre molte funzionalità avanzate per la gestione delle date e degli orari. Ad esempio, è possibile convertire una data da un fuso orario all'altro o confrontare due date per vedere quale è più recente utilizzando i metodi `astimezone()` e `timedelta()`.

Per ulteriori informazioni su queste funzioni e su altre funzionalità del modulo `datetime`, si consiglia di consultare la documentazione ufficiale di Python.

## Vedi anche

- La documentazione ufficiale di Python sul modulo datetime: https://docs.python.org/3/library/datetime.html
- Un tutorial dettagliato su come utilizzare il modulo datetime: https://www.programiz.com/python-programming/datetime

Grazie per aver letto questo post e speriamo che ora tu sia in grado di ottenere la data corrente utilizzando Python!