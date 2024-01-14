---
title:                "Elixir: Confrontare due date"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Molte volte, quando stiamo sviluppando un'applicazione o un sito web, potremmo aver bisogno di confrontare due date. Questo può essere necessario per verificare la corretta esecuzione di un'operazione o per gestire dati di tipo temporale. In questo articolo, ti mostreremo come confrontare due date in Elixir.

## Come Fare
Per confrontare due date in Elixir, abbiamo bisogno di una libreria chiamata `DateTime`. Questa libreria fornisce metodi utili per la manipolazione e la comparazione di date e orari.

Per iniziare, dobbiamo importare la libreria e creare due variabili che rappresentano le date che vogliamo confrontare.

```Elixir
import DateTime

date1 = DateTime.from_iso8601("2021-04-15")
date2 = DateTime.from_iso8601("2021-04-20")
```

Ora che le nostre date sono pronte, possiamo utilizzare il metodo `compare` per confrontarle. Questo metodo restituisce uno dei tre valori: `-1` se la prima data è precedente alla seconda, `0` se sono uguali o `1` se la prima data è successiva alla seconda.

```Elixir
DateTime.compare(date1, date2)
```

L'output sarà `-1` perché la prima data è precedente alla seconda. Se invece invertiamo l'ordine delle date, otterremo un output di `1`.

## Approfondimento
La libreria `DateTime` fornisce anche altri metodi utili per la manipolazione delle date, come `add`, `diff` e `trunc`.

Per esempio, possiamo utilizzare il metodo `add` per aggiungere un certo numero di giorni, settimane, mesi o anni a una data esistente.

```Elixir
DateTime.add(date1, [{:days, 10}])
```

Questo restituirà una data dieci giorni dopo la data iniziale. Possiamo anche utilizzare il metodo `diff` per calcolare la differenza in secondi, minuti, ore, giorni, settimane o anni tra due date.

Per maggiori informazioni sulla libreria `DateTime` e tutti i suoi metodi, ti consigliamo di consultare la documentazione ufficiale di Elixir.

## Vedi Anche
- [Documentazione ufficiale di Elixir - DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Articolo su come confrontare date in Elixir in inglese](https://www.poeticoding.com/how-to-compare-dates-in-elixir/)
- [Libreria Elixir per la manipolazione delle date](https://hex.pm/packages/timex)