---
title:    "Ruby: Läsa en textfil"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil kan vara en viktig del av programmering. Det kan hjälpa dig att behandla stora mängder data och automatisera uppgifter.

## Så här

För att läsa en textfil med Ruby, behöver du först öppna filen med "File.open ()" funktionen. Du kan sedan läsa varje rad i filen med "readline" metod. Nedan finns ett exempel:

```Ruby
File.open("textfil.txt", "r") do |f|
  f.each_line do |line|
    puts line
  end
end
```

Detta kodblock öppnar textfilen "textfil.txt" och läser sedan varje rad, som sedan skrivs ut på skärmen.

## Djupdykning

När du läser en textfil är det viktigt att ha i åtanke hur filen är formaterad. Om den är delimiterad, det vill säga separerad med ett särskilt tecken eller ett visst antal mellanrum, kan du använda "split" metod för att dela upp raden i olika delar. Detta kan vara användbart när du vill behandla specifika delar av varje rad.

En annan viktig aspekt är att hantera fel som kan uppstå när du läser en textfil. Det är viktigt att ha ett sätt att kontrollera om filen du öppnar verkligen finns och om du har rätt behörighet att läsa den.

## Se även

* [Ruby Dokumentation om File-klassen](https://ruby-doc.org/core-2.7.1/File.html)
* [Enkelt sätt att läsa en textfil med Ruby](https://www.codecademy.com/articles/read-files-ruby)
* [Lär dig mer om Ruby-programmering](https://www.ruby-lang.org/sv/)