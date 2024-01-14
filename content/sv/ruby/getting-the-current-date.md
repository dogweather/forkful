---
title:                "Ruby: Få den aktuella datumet"
simple_title:         "Få den aktuella datumet"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den nuvarande datumet är en viktig del av att skriva kod i Ruby, eftersom det är en viktig bit av information i många program. Genom att använda några enkla funktioner kan du få tillgång till det aktuella datumet och använda det i din kod.

## Så här gör du

För att få den nuvarande datumet, kan du använda funktionen `DateTime.now`. Detta kommer att returnera ett `DateTime` objekt som innehåller datumet och tiden just nu. Exempelvis:

```Ruby
nu = DateTime.now
```

Du kan också använda funktionen `Date.today` för att få tillgång till datumet utan tidsinformationen. Detta kan vara användbart om du bara behöver datumet och inte tiden. Exempelvis:

```Ruby
idag = Date.today
```

Båda dessa funktioner returnerar ett objekt som innehåller olika attribut såsom årtal, månad och dag. Du kan även manipulera datumet genom att använda olika metoderna som finns tillgängliga för dessa objekt. Exempelvis kan du använda `strftime` för att formatera datumet på olika sätt, eller `next_day` för att få nästa dag. 

## Djupdykning

Om du vill lära dig mer om hur man får den nuvarande datumet i Ruby, kan du läsa dokumentationen för `DateTime` och `Date` objekt, samt utforska andra funktioner som är tillgängliga för dessa objekt. Det finns också många resurser online, såsom bloggar och forum, där du kan lära dig mer och få hjälp om du stöter på problem.

## Se även

- [Dokumentation för DateTime och Date i Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/date/rdoc/DateTime.html)
- [En guide för hur man använder datum och tid i Ruby](https://www.rubyguides.com/2015/07/ruby-datetime/)
- [Ett forum för Ruby-programmerare](https://ruby-forum.com/)