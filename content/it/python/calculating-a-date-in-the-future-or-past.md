---
title:    "Python: Calcolare una data nel futuro o nel passato"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per pianificare eventi, gestire scadenze o semplicemente per soddisfare la nostra curiosità.

## Come fare

```python
# Importa il modulo datetime
import datetime

# Definisci la data di oggi
oggi = datetime.date.today()

# Calcola una data nel futuro aggiungendo 30 giorni a oggi
futuro = oggi + datetime.timedelta(days=30)

# Calcola una data nel passato sottraendo 15 giorni a oggi
passato = oggi - datetime.timedelta(days=15)

# Stampa i risultati
print("Data oggi:", oggi)
print("Data nel futuro:", futuro)
print("Data nel passato:", passato)
```
### Output:
```
Data oggi: 2021-05-01
Data nel future: 2021-05-31
Data nel passato: 2021-04-16
```

## Approfondimento

Il modulo datetime è utile per gestire date e orari in Python. Per calcolare una data nel futuro o nel passato, si utilizza la funzione timedelta, che consente di aggiungere o sottrarre un determinato numero di giorni, ore, minuti o secondi a una data specifica. Inoltre, il modulo datetime offre molte altre funzioni per manipolare e formattare le date.

## Vedi anche

- Introduzione al modulo datetime in Python: [https://www.geeksforgeeks.org/python-datetime-module/](https://www.geeksforgeeks.org/python-datetime-module/)
- Documentazione ufficiale del modulo datetime: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)