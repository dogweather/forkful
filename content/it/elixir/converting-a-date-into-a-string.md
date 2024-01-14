---
title:    "Elixir: Convertire una data in una stringa"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Perché

In programmazione è molto comune dover manipolare le date e spesso abbiamo la necessità di convertire una data in una stringa. Ciò può essere utile, ad esempio, per mostrare la data su un'interfaccia utente o per salvare una data in un formato leggibile dai nostri database. In Elixir, ci sono diverse funzioni che ci permettono di effettuare questa conversione in modo facile e preciso.

# Come Fare

Per convertire una data in una stringa in Elixir, utilizzeremo la funzione ```NaiveDateTime.to_string/2```, dove il primo argomento è la data da convertire e il secondo è il formato in cui vogliamo ottenere la stringa. Vediamo un esempio pratico:

```
Elixir
my_date = ~N[2019-09-14 13:30:00]
NaiveDateTime.to_string(my_date, "{YYYY}-{MM}-{DD} {hh}:{mm}")
```

In questo caso, la funzione restituirà la stringa "2019-09-14 13:30". Possiamo anche specificare altri formati, come ad esempio "{YYYY}/{MM}/{DD} {hh}:{mm}:{ss}" che restituirebbe "2019/09/14 13:30:00". È possibile trovare una lista completa dei formati disponibili nella documentazione di Elixir.

# Approfondimento

Quando si converte una data in una stringa, bisogna fare attenzione al fuso orario in cui si sta lavorando. In Elixir, le date sono rappresentate in formato UTC, quindi se siamo in un fuso orario diverso, dovremo fare una conversione prima di convertire la data in stringa. Possiamo utilizzare la funzione ```DateTime.convert/3``` per questo scopo, specificando come argomenti la data, il fuso orario di origine e quello di destinazione.

Un'altra cosa importante da tenere a mente è la presenza di valori nulli nei nostri dati. Se la data dovesse essere nulla, la funzione di conversione restituirebbe una stringa vuota, quindi dobbiamo gestire questo caso nella nostra logica di programmazione.

# Vedi Anche

- Documentazione delle funzioni di conversione delle date in Elixir: https://hexdocs.pm/elixir/NaiveDateTime.html#to_string/2
- Elixir Date Scegliere le operazioni dei Pacchetti: https://www.elixirpackages.com/c/date/
- Formato delle Date ISO 8601: https://it.wikipedia.org/wiki/ISO_8601#Data_e_orario