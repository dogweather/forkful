---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument är att hämta data direkt från användaren när de startar ett program. Programmerare gör detta för att göra sina program mer flexibla och användardrivna.

## Så här gör du:
Din Ruby kod kan acceptera kommandoradsargument via ett globalt array som heter ARGV. Argumenten kan sedan läsas som strängar. Nedan följer ett exempel på hur detta kan se ut.
```Ruby
puts "Du angav #{ARGV.length} argument:"
ARGV.each_with_index do |val, index|
  puts "Argument #{index + 1}: #{val}"
end
```
Om du kör kodsnutten ovan med argumenten "hej" "där" "världen" (ruby arg_example.rb hej där världen) kommer output att bli:
```Ruby
Du angav 3 argument:
Argument 1: hej
Argument 2: där
Argument 3: världen
```

## Djupdykning
Kommandoradsargument har en lång historia, som sträcker sig tillbaka till de tidiga dagarna av Unix. De används fortfarande idag i många olika programmeringsspråk, inklusive Ruby, för att erbjuda användarfunktionalitet direkt vid start av program. 

Alternativ till kommandoradsargument i Ruby inkluderar att använda standard input (`STDIN.gets`), vilket gör det möjligt att hämta data från användaren efter att ett program har startats, eller använda en konfigurationsfil.

En viktig detalj att notera om ARGV i Ruby är att den inte inkluderar det första argumentet (skriptnamnet / filnamnet). Detta är en konvention som kommer från C och Unix, och har bevarats i Ruby för kompatibilitet och förväntan.

## Se Även
1. [ARGV i Ruby](https://ruby-doc.org/core-2.7.0/ARGV.html) - RubyDOC om ARGV global array, med fler exempel och detaljer.
2. [Kommandoradsargument i UNIX](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html) - En historisk översyn av argument syntax i UNIX, där ideerna för kommandoradsargument ursprungligen kommer ifrån.
3. [Kommandoargument vs stdin](http://www.linfo.org/standard_input.html) - En jämförelse mellan kommandoradargument och standard input, från Linux Information Project.