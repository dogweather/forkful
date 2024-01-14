---
title:                "Elixir: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché 

Comparare due date è un'operazione comune nella programmazione e può essere utile in molte situazioni diverse. Ad esempio, può essere necessario confrontare due date per verificare quale è la più recente o per controllare se una determinata data è compresa in un intervallo di tempo specifico. In questo articolo, esploreremo come eseguire il confronto di due date usando Elixir.

## Come fare

Elixir offre diversi metodi per confrontare due date. Uno dei metodi più semplici è utilizzare l'operatore di uguaglianza `==`, che restituirà `true` se le due date sono identiche e `false` altrimenti. Vediamo un esempio pratico:

```
Elixir  def date1 = %{day: 1, month: 1, year: 2020}
def date2 = %{day: 1, month: 1, year: 2020}
date1 == date2
```

Questo codice restituirebbe `true` poiché le due date sono identiche. Tuttavia, se vogliamo confrontare le date in base alla loro posizione cronologica, possiamo utilizzare l'operatore `>` o `<`. Ad esempio:

```
  Elixir  def date3 = %{day: 2, month: 1, year: 2020}
  def date4 = %{day: 1, month: 1, year: 2020}
  date3 > date4
```

In questo caso, il codice restituirebbe `true` poiché la data 3 è più recente della data 4.

Oltre agli operatori, Elixir offre anche diverse funzioni per confrontare le date, come `Date.compare/2` e `Date.same_date?/2`. È possibile esplorare queste opzioni nella documentazione ufficiale di Elixir.

## Approfondimento

Durante il confronto di due date è importante tenere conto di diversi fattori, come la formattazione della data e il fuso orario. Inoltre, è possibile confrontare le date in base a un singolo componente, come il giorno o il mese, utilizzando il modulo `Calendar` di Elixir.

È inoltre possibile confrontare le date usando timestamp al posto delle date di calendario. Per fare ciò, è possibile utilizzare la funzione `DateTime.to_unix/1` per convertire le date in timestamp e quindi confrontarle utilizzando gli operatori di confronto standard.

## Vedi anche

- Documentazione ufficiale di Elixir sull'operatore di uguaglianza: https://hexdocs.pm/elixir/Kernel.html#==/2
- Documentazione ufficiale di Elixir sul modulo Calendar: https://hexdocs.pm/elixir/Calendar.html
- Documentazione ufficiale di Elixir sulla funzione DateTime.to_unix/1: https://hexdocs.pm/elixir/DateTime.html#to_unix/1