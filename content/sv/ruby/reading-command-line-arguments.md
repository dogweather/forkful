---
title:                "Ruby: Läsa kommandoradsargument"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför

Det finns många anledningar till varför man skulle vilja läsa inkommande kommandoradsargument i ett Ruby-program. Det kan hjälpa dig att skriva mer interaktiva och anpassningsbara program, samla data från användare eller automatisera processer. Det är också en användbar färdighet för att ha i din programmeringsverktygslåda.

# Hur man gör

Att läsa kommandoradsargument i Ruby är ganska enkelt. Du behöver bara använda ARGV-objektet och dess metoder för att samla in argumenten. Här är ett exempel på kod:

```Ruby
# Sparar alla argument i en array
arguments = ARGV

# Skriver ut det första argumentet
puts "Hej #{arguments[0]}!"
```

Om du kör detta program med kommandot `ruby hello.rb världen` kommer det att skriva ut "Hej världen!". Det är värt att notera att det första elementet i ARGV-objektet alltid är namnet på programmet som körs.

Du kan också använda ARGV-metoden `shift` för att ta bort det första argumentet från arrayen och få ut alla resterande argument. Här är ett exempel:

```Ruby
# Sparar alla argument i en array
arguments = ARGV

# Skriver ut alla argument förutom det första
puts "Hej #{arguments.shift}!"

# Skriver ut alla återstående argument
arguments.each do |argument|
  puts "#{argument} är också här!"
end
```

Kör detta med samma kommando som ovan och det kommer att skriva ut "Hej världen!", följt av "världen är också här!".

# Djupdykning

ARGV-objektet innehåller också den ursprungliga kommandoradssträngen som skickades in till programmet. Detta kan vara användbart om du vill hitta ett specifikt argument eller göra mer avancerade manipulationer med kommandoradsargumenten.

En annan användbar funktion är att du kan använda ARGV för att ange förväntat antal argument och skriva ut ett felmeddelande om användaren skickar in fel antal argument. Till exempel:

```Ruby
# Kontrollerar om antalet argument är 2
if ARGV.length != 2
  puts "Vänligen ange två argument."
else
  # Lager logik för att använda de två argumenten
end
```

Med detta i åtanke kan du nu börja utforska alla möjligheter som läsning av kommandoradsargument erbjuder dig när du skriver dina Ruby-program.

# Se även

- [Dokumentation för ARGV-objektet](https://ruby-doc.org/core-2.6.5/ARGF.html)
- [Exempel på kommandoradsargument i Ruby-program](https://www.rubyguides.com/2012/02/ruby-command-line-arguments/)