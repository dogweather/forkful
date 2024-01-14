---
title:    "Python: Ottenere la data corrente"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Perché

Le date sono un elemento importante nella programmazione, in particolare quando si devono gestire diversi processi e attività in base al giorno attuale. Con Python, è possibile ottenere facilmente la data e utilizzarla per automatizzare alcune funzioni nel tuo codice.

##Come Fare

Per ottenere la data attuale in Python, è necessario importare il modulo "datetime". In questo modo, è possibile accedere a diverse funzioni per gestire le date. Ecco un esempio di codice che stampa la data corrente:

```Python
import datetime

today = datetime.date.today()
print(today)
```

L'output dovrebbe essere qualcosa del genere:

```Python
2021-08-04
```

In questo modo, si ottiene la data attuale nel formato anno-mese-giorno. Inoltre, è possibile ottenere la data e l'ora attuali utilizzando la funzione "datetime.now()". Ecco un esempio di codice che stampa la data e l'ora correnti:

```Python
import datetime

now = datetime.datetime.now()
print(now)
```

L'output dovrebbe essere qualcosa del genere:

```Python
2021-08-04 15:30:00.951582
```

Inoltre, è possibile formattare la data in diversi modi utilizzando il metodo "strftime()". Ad esempio, se si vuole avere la data nel formato giorno/mese/anno, è possibile utilizzare il seguente codice:

```Python
import datetime

today = datetime.date.today()
formatted_date = today.strftime("%d/%m/%Y")
print(formatted_date)
```

L'output dovrebbe essere:

```Python
04/08/2021
```

##Approfondimento

Ottenere la data attuale può sembrare semplice, ma è importante comprendere come funziona il modulo "datetime" per gestire le date e le ore nel tuo codice. Inoltre, è possibile utilizzare diverse opzioni di formattazione e funzioni per trattare le date in modo più avanzato, ad esempio per eseguire calcoli o confronti tra date diverse.

##Vedi anche

- Documentazione ufficiale del modulo datetime: https://docs.python.org/3/library/datetime.html
- Tutorial su come utilizzare il modulo datetime: https://www.programiz.com/python-programming/datetime
- Altro esempio di formattazione delle date: https://www.w3schools.com/python/python_datetime.asp