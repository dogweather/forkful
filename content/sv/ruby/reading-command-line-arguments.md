---
title:                "Ruby: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Kommandoradsargument är en viktig aspekt av programmering som kan hjälpa dig att skräddarsy och kontrollera ditt program utan att ständigt behöva ändra koden. Genom att läsa på om kommandoradsargument kan du göra dina program mer dynamiska och anpassningsbara.

## Hur man gör

För att läsa kommandoradsargument i ett Ruby-program, börjar du med att skriva in "ARGV" i din kod. Detta är en array som innehåller argumenten som skickas till ditt program från kommandoraden. Du kan sedan iterera genom arrayen eller använda specifika index för att hämta specifika argument.

``` ruby
ARGV.each do |arg|
  puts "Argument: #{arg}"
end
```

Om du till exempel kör ditt program med kommandot "ruby program.rb hello world", kommer output att bli:

```
Argument: hello
Argument: world
```

Du kan också använda index för att hämta specifika argument. Om du vill hämta det andra argumentet (i det här fallet "world") kan du använda ARGV[1].

``` ruby
puts "Andra argumentet: #{ARGV[1]}"
```

## Djupdykning

Det finns många olika sätt att använda och hantera kommandoradsargument i Ruby. Du kan till exempel använda flaggor för att lägga till specifika funktioner till ditt program eller använda gem för att underlätta läsning och hantering av argumenten.

Det är också viktigt att notera att ARGV inte bara kan användas för att läsa kommandoradsargument från en terminal. Det kan också läsas från andra filer eller inputströmmar. Det är en kraftfull och allsidig funktion i Ruby-programmering.

## Se även

- [Ruby Dokumentation: Kommandoradsargument](https://ruby-doc.org/docs/ProgrammingRuby/html/tut_stdtutorial.html)
- [Ruby CLI Gem](https://github.com/cldwalker/hirb-unicode)
- [Användning av ARGV i Ruby-program](https://dev.to/iriskosari/argv-in-ruby-20j7)