---
title:                "Elixir: Calcolare una data nel futuro o nel passato"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Perché
In questo articolo scoprirete come calcolare una data nel futuro o nel passato utilizzando il linguaggio di programmazione Elixir. Questa abilità è particolarmente utile per creare applicazioni che richiedono il calcolo di date in modo dinamico.

# Come
Utilizzando la libreria `Calendar`, è possibile calcolare facilmente una data nel futuro o nel passato. Per calcolare una data nel futuro, è necessario specificare la data di partenza, l'unità di tempo (giorni, mesi o anni) e la quantità di tempo da aggiungere. Ecco un esempio di codice:

```Elixir
start_date = ~D[2021-01-01]
end_date = Calendar.shift(start_date, :days, 10)
IO.puts(end_date) # Output: 2021-01-11
```

Per calcolare una data nel passato, è sufficiente specificare una quantità di tempo negativa, come mostrato nell'esempio seguente:

```Elixir
start_date = ~D[2021-01-11]
end_date = Calendar.shift(start_date, :months, -2)
IO.puts(end_date) # Output: 2020-11-11
```

# Deep Dive
La libreria `Calendar` utilizza il calendario gregoriano per calcolare le date. Ciò significa che le date precedenti al 15 ottobre 1582 possono non essere accurate a causa dell'adozione del calendario gregoriano in diverse nazioni in momenti diversi. Inoltre, la precisione delle date diminuisce quando si calcolano intervalli di tempo molto ampi, come ad esempio l'aggiunta di 1000 anni.

È inoltre possibile utilizzare la funzione `Calendar.add` per calcolare una data aggiungendo un numero specifico di giorni, mesi o anni a una data di partenza. Ad esempio:

```Elixir
start_date = ~D[2021-01-01]
end_date = Calendar.add(start_date, {:months, 6})
IO.puts(end_date) # Output: 2021-07-01
```

# Vedi anche
- [Documentazione ufficiale di Elixir sulla libreria Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Articolo su come calcolare date con Elixir](https://pragmaticstudio.com/tutorials/matching-patterns-with-elixir/working-with-date-and-time)
- [Ulteriori informazioni sulla storia del calendario gregoriano](https://www.timeanddate.com/calendar/gregorian-calendar.html)