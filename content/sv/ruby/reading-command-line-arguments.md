---
title:                "Läsning av kommandoradsargument"
html_title:           "Ruby: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Rubeshestayelsenues läser kommandoradsargument kan vara en hjälpsam färdighet att ha vid utveckling av Ruby-program. Det tillåter programmet att interagera med användaren genom att acceptera input från terminalen och utföra önskade åtgärder. 

## Så här gör du
Att läsa kommandoradsargument i Ruby är en relativt enkel process. Det första steget är att definiera variabler som kommer att hålla argumenten som programmet tar emot. Detta kan göras genom att använda ARGV-variabeln, vilket ger en array av argument som skickas från terminalen till programmet.

```Ruby
arguments = ARGV
```

Efter att ha definierat variabeln kan vi sedan använda ruby-metoden `each` för att iterera genom argumenten och utföra önskade åtgärder.

```Ruby
arguments.each do |arg|
  # utför önskad åtgärd med varje argument
end
```

Om programmet förväntar sig en viss typ av argument, till exempel en sträng eller ett heltal, kan vi använda `to_s` eller `to_i` för att omvandla argumentet till rätt typ innan vi utför åtgärden.

```Ruby
arguments.each do |arg|
  num = arg.to_i
  # utför önskad åtgärd som kräver ett heltal
end
```

Om användaren inte skickar med några argument i terminalen kommer ARGV-variabeln att vara en tom array. Detta kan utnyttjas för att kontrollera om användaren har gett nödvändig input.

```Ruby
if arguments.empty?
  puts "Inga argument skickades med."
else
  arguments.each do |arg|
    # utför önskad åtgärd
  end
end
```

## Djupdykning
När vi läser kommandoradsargument i Ruby är det viktigt att känna till hur argumenten är strukturerade. Argumenten delas upp vid mellanslag och sparas sedan som strängar. Om ett argument innehåller mellanslag måste det omges av citat-tecken för att behålla sin integritet som ett enda argument.

En annan användbar metod för att hantera kommandoradsargument är `getoptlong`. Denna metod möjliggör för användaren att skicka med flaggor och värden som sedan kan läsas genom att använda methodName- och optName-variabler.

```Ruby
require 'getoptlong'

opts = GetoptLong.new(
  ["--input", "-i", GetoptLong::OPTIONAL_ARGUMENT],
  ["--output", "-o", GetoptLong::OPTIONAL_ARGUMENT]
)

optName = ""
methodName = ""

opts.each do |option, arg|
  case option
  when "--input"
    optName = "input"
    methodName = "input_method"
  when "--output"
    optName = "output"
    methodName = "output_method"
  end
end

puts "Värdet för #{optName} är #{methodName}."
```

Det finns också många andra användbara metoder som kan användas för att hantera kommandoradsargument, som exempelvis `OptionParser` och `ARGV.shift`. Det är en bra idé att undersöka alla tillgängliga alternativ och välja den metod som passar bäst för det specifika projektet.

## Se också
* [Ruby ARGV](https://ruby-doc.org/core-2.7.0/ARGV.html)
* [Ruby getoptlong](https://ruby-doc.org/stdlib-2.7.0/libdoc/getoptlong/rdoc/GetoptLong.html)
* [Ruby OptionParser](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
* [Ruby ARGV.shift](https://ruby-doc.org/core-2.7.0/ARGF.html#method-c-shift)