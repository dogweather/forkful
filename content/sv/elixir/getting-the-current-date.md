---
title:                "Hämta nuvarande datum"
html_title:           "Elixir: Hämta nuvarande datum"
simple_title:         "Hämta nuvarande datum"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna får dagens datum är en grundläggande funktion som många program behöver. Oavsett om man bygger en kalenderapplikation eller ett bokningssystem, är det viktigt att kunna få den nuvarande datumen för att skapa en användarvänlig och strukturerad upplevelse.

## Så här gör du

För att få det nuvarande datumet i Elixir, finns det en inbyggd funktion som heter `Date.utc_today()`. Genom att använda denna funktion får man tillbaka en sträng i formatet YYYY-MM-DD, vilket är standardformatet för datum och tid i ISO 8601. Här är ett exempel på hur man använder funktionen:

```Elixir
current_date = Date.utc_today()
IO.puts "Idag är det " <> current_date
```

Det här skulle resultera i output: "Idag är det 2021-09-14". Genom att använda `<>` operatören, kan vi sammanfoga strängar i Elixir.

## Djupdykning

Det finns också andra sätt att få det nuvarande datumet i Elixir. En annan inbyggd funktion som är tillgänglig är `Date.utc_now()`, som returnerar datumet och tiden i UTC-tidzon. Denna funktion tar också emot ett argument för att ange den önskade tidszonen. Här är ett exempel:

```Elixir
current_datetime = Date.utc_now("Europe/Stockholm")
IO.puts "Nu är det " <> current_datetime
```

Det här skulle ge output: "Nu är det 2021-09-14 15:00:00.000000Z", baserat på den nuvarande tiden i Stockholm, Sverige.

Det finns också ytterligare bibliotek som erbjuder mer avancerade funktioner för att hantera datum och tid i Elixir, såsom "timex" och "calendar" biblioteken. Dessa kan vara värdefulla att utforska om man behöver mer specifika datum- och tidsfunktioner.

## Se även

- Officiell dokumentation för `Date` modulen i Elixir: https://hexdocs.pm/elixir/Date.html
- "timex" biblioteket för mer avancerad datum och tidsbehandling: https://hexdocs.pm/timex/readme.html
- "calendar" biblioteket för datum och tidsberäkningar: https://hexdocs.pm/calendar/readme.html