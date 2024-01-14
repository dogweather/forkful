---
title:    "Elixir: Beräkna ett datum i framtiden eller det förflutna"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Du kanske undrar varför du skulle vilja räkna ut ett datum i framtiden eller förflutna? Det finns många användbara tillämpningar för denna funktion, såsom att planera möten, hantera prenumerationer eller för att utveckla algoritmer för tidshantering.

## Hur man gör det

För att räkna ut ett datum i framtiden eller förflutna i Elixir, använder man funktionen `DateTime.add/2`. Till exempel om vi vill ha datumet för en vecka framåt från idag, skulle koden se ut så här: 

```
Elixir DateTime.add(DateTime.utc_now(), 7, :days)
```

Detta kommer att returnera ett `DateTime` -objekt med det nya datumet. Om vi vill ha datumet för en månad framåt, skulle koden se ut så här:

```
Elixir DateTime.add(DateTime.utc_now(), 1, :months)
```

För att få datumet för ett förflutet datum, använder man istället en negativ siffra för andra parametern. Till exempel, om vi vill ha datumet för en vecka tillbaka från idag:

```
Elixir DateTime.add(DateTime.utc_now(), -7, :days)
```

Det är också möjligt att ange mer än en tidsenhet i andra parameter, till exempel `DateTime.add(DateTime.utc_now(), 2, :months, 1, :weeks)` skulle returnera datumet för två månader och en vecka framåt.

## Djupdykning

Nu när vi vet hur man beräknar datum i framtiden eller förflutna, kan vi titta på några vanliga användningar av denna funktion. En vanlig användning är att planera möten eller händelser. Om du behöver boka ett möte en månad framåt, kan du använda `DateTime.add` för att få det exakta datumet. En annan vanlig användning är för prenumerationer. Genom att använda denna funktion kan du beräkna när en prenumeration kommer att förfalla och skicka en påminnelse till användaren.

## Se även

* [Official Elixir Documentation on DateTime](https://hexdocs.pm/elixir/DateTime.html)
* [Elixir DateTime Tutorial](https://elixir-lang.org/getting-started/datetime.html)
* [Date and Time Functions in Elixir](https://thoughtbot.com/blog/date-and-time-in-elixir)