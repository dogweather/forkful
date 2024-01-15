---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "C#: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché 

Calcolare una data nel futuro o nel passato può essere utile per vari scopi, ad esempio per pianificare eventi o per gestire la scadenza di una specifica attività. In questo articolo, vedremo come farlo utilizzando il linguaggio di programmazione C#.

## Come fare

Per calcolare una data nel futuro o nel passato in C#, possiamo utilizzare la classe `DateTime` del framework .NET. Questa classe ci permette di manipolare date e orari in modo semplice ed efficiente.

### Calcolare una data futura

Per calcolare una data futura, possiamo utilizzare il metodo `Add` della classe `DateTime`, passando come parametro l'intervallo di tempo desiderato. Ad esempio, se vogliamo ottenere la data di oggi più 15 giorni, possiamo scrivere il seguente codice:

```C#
DateTime oggi = DateTime.Today; // ottiene la data di oggi
DateTime dataFutura = oggi.Add(TimeSpan.FromDays(15)); // aggiunge 15 giorni
Console.WriteLine(dataFutura); // output: 27/03/2021
```

Nell'esempio sopra, stiamo utilizzando il metodo `FromDays` della classe `TimeSpan` per creare un intervallo di tempo di 15 giorni. Questo valore viene poi passato come parametro al metodo `Add` della classe `DateTime`, che ci permette di ottenere la data futura desiderata.

### Calcolare una data passata

Per calcolare una data passata, possiamo utilizzare lo stesso approccio utilizzato per la data futura, ma invece di aggiungere un intervallo di tempo, possiamo sottrarlo. Ad esempio, se vogliamo ottenere la data di oggi meno 1 mese, possiamo scrivere il seguente codice:

```C#
DateTime oggi = DateTime.Today; // ottiene la data di oggi
DateTime dataPassata = oggi.Add(-1 * TimeSpan.FromMonths(1)); // sottrae 1 mese
Console.WriteLine(dataPassata); // output: 28/01/2021
```

Come vediamo nell'esempio sopra, stiamo utilizzando il metodo `FromMonths` della classe `TimeSpan` per creare un intervallo di tempo di 1 mese, che viene poi sottratto dalla data di oggi utilizzando il segno meno (-) davanti all'intervallo.

## Approfondimenti

La classe `DateTime` offre molti altri metodi e proprietà utili per la manipolazione di date e orari. Ad esempio, possiamo ottenere il giorno della settimana di una data specifica utilizzando il metodo `DayOfWeek` o convertire una data in una stringa formattata utilizzando il metodo `ToString`.

Inoltre, è possibile utilizzare la classe `TimeSpan` per creare intervalli di tempo personalizzati, utili per calcolare date in modo più preciso.

## Vedi anche

- [Guida completa alla classe `DateTime` (Microsoft Docs)](https://docs.microsoft.com/it-it/dotnet/api/system.datetime)
- [Guida completa alla classe `TimeSpan` (Microsoft Docs)](https://docs.microsoft.com/it-it/dotnet/api/system.timespan)