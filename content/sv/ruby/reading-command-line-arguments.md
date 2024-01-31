---
title:                "Läsa in kommandoradsargument"
date:                  2024-01-20T17:56:42.038014-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument är processen att fånga data som användaren skickar in till ditt program från terminalen. Vi gör det för att tillåta användaren att justera programmet's beteende eller för att mata in viktig information utan att behöva ändra koden.

## Hur gör man:
Ruby har en enkel och rättfram syn på kommandoradsargument genom att använda den globala arrayen `ARGV`. Här är ett grundläggande exempel:

```ruby
# hello.rb
puts "Hej #{ARGV[0]}!"
```

Kör detta genom att skriva `ruby hello.rb Världen` i terminalen, och resultatet blir:

```ruby
Hej Världen!
```

Om du vill hantera flera argument, iterera bara över `ARGV`:

```ruby
# greeter.rb
ARGV.each do |arg|
  puts "Hej #{arg}!"
end
```

Kör med `ruby greeter.rb Världen Universum`, skulle producera:

```ruby
Hej Världen!
Hej Universum!
```

## Fördjupning
Historiskt sett har kommandoradsargument varit en viktig del av interaktiva skript och program. I Ruby's värld är de enkla att använda men kraftfulla och hanteras via `ARGV` arrayen som automatiskt populeras med argumenten som skickas till ditt skript.

Om du behöver mer avancerad argumenthantering kan du använda inbyggda bibliotek som `OptionParser` eller externa gems som `Thor` för att hantera flaggor och komplexa behov.

För att förstå hur Ruby isolerar dessa argument från programmets interna variabler är det värt att notera att `ARGV` och `$*` är alias för samma objekt, vilket illustrerar kopplingen mellan Ruby och dess Unix-ursprung där `$*` används för att referera till alla skriptargument i shell-skript.

Det kan vara frestande att använda `gets.chomp` direkt för enkel indata, men undvik det här för argument som ska kunna hanteras med skriptet kallat; det är inte vad `gets` är avsett för.

## Se även
- Ruby's dokumentation om ARGV: https://ruby-doc.org/core-2.7.0/ARGV.html
- Ruby's dokumentation om OptionParser: https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html
- GitHub-sidan för Thor-gem: https://github.com/rails/thor
