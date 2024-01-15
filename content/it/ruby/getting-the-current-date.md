---
title:                "Ottenere la data attuale"
html_title:           "Ruby: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui qualcuno potrebbe voler ottenere la data attuale in un programma Ruby. Ad esempio, potrebbe essere necessario per creare una funzione che registra l'ora di un'azione o per stampare la data in un formato specifico.

## Come fare
Per ottenere la data attuale in Ruby, possiamo utilizzare il metodo `Time.now`. Possiamo anche utilizzare il metodo `Date.today` per ottenere la data come oggetto di tipo `Date`. Ecco un esempio di codice e output:

```Ruby
# Utilizzando il metodo Time.now
puts Time.now

# Output:
# 2021-11-12 11:33:45 +0100

# Utilizzando il metodo Date.today
puts Date.today

# Output:
# 2021-11-12
```

## Approfondimento
Il metodo `Time.now` restituisce un oggetto di tipo `Time`, che rappresenta un momento specifico nel tempo, con informazioni su ore, minuti, secondi e fuso orario. Il metodo `Date.today`, invece, restituisce un oggetto di tipo `Date`, che rappresenta semplicemente una data senza informazioni sul tempo.

Entrambi i metodi utilizzano l'orologio interno del sistema operativo per ottenere la data e l'ora attuali. Se si desidera ottenere la data in un formato diverso da quello di default (come nel formato "giorno/mese/anno"), è possibile utilizzare il metodo `strftime`.

Ecco un esempio di codice che utilizza `strftime` per ottenere la data nel formato "giorno/mese/anno":

```Ruby
puts Time.now.strftime("%d/%m/%Y")

# Output:
# 12/11/2021
```

## Vedi anche
- Metodo `Time.now`
- Metodo `Date.today`