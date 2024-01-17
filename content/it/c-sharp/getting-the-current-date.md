---
title:                "Ottener"
html_title:           "C#: Ottener"
simple_title:         "Ottener"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Ottenere la data corrente è una funzione comune nella programmazione per ottenere informazioni sulla data attuale. I programmatori spesso utilizzano questa funzionalità per tenere traccia dei record, creare registri di attività o semplicemente per fornire informazioni accurate all'utente.

## Come fare:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```
Output:
```
11/18/2021 15:30:00
```

Per ottenere la data corrente in C#, basta utilizzare la classe ```DateTime``` e il metodo ```Now```. Questo restituirà la data e l'ora correnti come un oggetto di tipo ```DateTime```. È anche possibile formattare l'output per ottenere solo la data o solo l'ora, a seconda delle necessità.

## Approfondimento:

La classe ```DateTime``` è stata introdotta in .NET Framework per gestire date e orari nella programmazione. Inoltre, è possibile utilizzare il metodo ```Today``` per ottenere solo la data corrente senza l'informazione sull'ora. 

Un'alternativa a questo approccio può essere l'utilizzo della classe ```DateTimeOffset```, che rappresenta una data e un'ora specificando sia il fuso orario che l'offset rispetto all'ora UTC. In alternativa, è possibile utilizzare librerie di terze parti come NodaTime per gestire tutte le operazioni legate alla data e all'ora in modo più flessibile.

Per quanto riguarda l'implementazione, il metodo ```Now``` utilizza il fuso orario del sistema operativo per ottenere la data e l'ora correnti. È possibile specificare un fuso orario diverso utilizzando il metodo ```Now.ToLocalTime()``` per ottenere l'ora locale o ```Now.ToUniversalTime()``` per ottenere l'ora UTC.

## Vedi anche:

- [Documentazione Microsoft su DateTime](https://docs.microsoft.com/it-it/dotnet/api/system.datetime?view=net-5.0)
- [Libreria NodaTime](https://nodatime.org/)
- [Documentazione Microsoft su DateTimeOffset](https://docs.microsoft.com/it-it/dotnet/api/system.datetimeoffset?view=net-5.0)