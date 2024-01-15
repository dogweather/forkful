---
title:                "Convertire una data in una stringa"
html_title:           "Ruby: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa può essere utile quando si lavora con date in un formato specifico o quando si vuole visualizzare una data in un formato più leggibile per gli utenti.

## Come fare

Per convertire una data in una stringa in Ruby puoi utilizzare il metodo `strftime()` della classe `Time` o `Date`. Questi metodi accettano un argomento che rappresenta il formato della stringa di output. Ad esempio:

```Ruby
today = Time.now
puts today.strftime("%d/%m/%Y")
```

Questo codice stampa la data corrente in formato giorno/mese/anno.

## Approfondimento

La classe `Time` rappresenta un orario specifico, mentre `Date` rappresenta una data senza un orario specifico. Il formato della stringa utilizzato nel metodo `strftime()` è basato sulle direttive presenti nella libreria C standard `strftime()`. Queste direttive indicano come la stringa di output dovrà essere formattata in base ai componenti della data, come il giorno, il mese e l'anno. Ad esempio:

| Direttiva | Descrizione      | Esempio          | Output      |
| :-------- | :--------------- | :--------------- | :---------- |
| %d        | Giorno del mese  | %d/%m/%Y        | 30/06/2021  |
| %m        | Mese             | %B %Y           | June 2021   |
| %Y        | Anno con 4 cifre | %d/%m/%Y        | 30/06/2021  |
| %b        | Nome del mese    | %d %b %Y        | 30 Jun 2021 |

Puoi combinare diverse direttive per ottenere un formato personalizzato per la tua stringa di output.

## Vedi anche

- [Documentazione di Ruby su Time](https://ruby-doc.org/core-3.0.0/Time.html)
- [Documentazione di Ruby su Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Tabella delle direttive di `strftime()`](http://man7.org/linux/man-pages/man3/strftime.3.html)