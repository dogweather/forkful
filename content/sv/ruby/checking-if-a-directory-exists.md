---
title:    "Ruby: Hitta om en mapp finns"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av Ruby-programmering. Det kan vara användbart för att se till att nytt innehåll inte skrivs över i befintliga mappar, eller för att se till att nödvändiga mappar finns innan en kod exekveras. Det är också en viktig säkerhetsfunktion för att förhindra oavsiktlig användning av icke-existerande mappar.

## Så här gör du

För att kontrollera om en mapp existerar, behöver du använda ett inbyggt Ruby-kommando som heter `Dir.exist?()`. Detta kommando tar en sökväg som argument och returnerar antingen sant (true) eller falskt (false) beroende på om mappen finns eller inte.

Exempel:

```Ruby
if Dir.exist?("Dokument/mapp")
  puts "Mappen finns!"
else
  puts "Mappen finns inte."
end
```

Output:

```
Mappen finns!
```

Om mappen inte finns, kommer output istället att vara:

```
Mappen finns inte.
```

## Djupdykning

När du använder `Dir.exist?()` är det viktigt att notera att det endast kontrollerar om en mapp existerar, inte om den är åtkomlig. Det betyder att det inte garanterar att du kommer att kunna läsa eller skriva i mappen. För att kontrollera både existens och tillgänglighet av en mapp, behöver du använda `test()` metoden på `File`-klassen.

Exempel:

```Ruby
if File.test("/hem/användare/mapp", "w")
  puts "Du kan skriva till mappen!"
else
  puts "Mappen är inte tillgänglig för skrivning."
end
```

Output:

```
Du kan skriva till mappen!
```

En annan viktig sak att tänka på är att `Dir.exist?()` kommer att returnera `true` även om sökvägen leder till en fil istället för en mapp. För att exakt kontrollera om det är en mapp, kan du använda `File.directory?()`.

Exempel:

```Ruby
if File.directory?("Dokument/fil.txt")
  puts "Detta är inte en mapp."
else
  puts "Detta är en mapp."
end
```

Output:

```
Detta är en mapp.
```

## Se också

- [Ruby API-dokumentation om Dir.exist?()](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [Ruby API-dokumentation om File.test()](https://ruby-doc.org/core-2.7.1/File.html#method-c-test)
- [Ruby API-dokumentation om File.directory?()](https://ruby-doc.org/core-2.7.1/File.html#method-c-directory-3F)