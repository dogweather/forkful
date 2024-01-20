---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skapa en temporär fil innebär att du tillfälligt lagrar data i en filtext. Programmerare gör detta när de behöver manipulera data utan att påverka den ursprungliga filen.

## Hur Man Gör:

Här är en hur du kan skapa och använda en temporär fil i Ruby.

```ruby
require 'tempfile'

# Skapa en temporär fil
temp_file = Tempfile.new('my_temp_file')

# Skriv till filen
temp_file.puts("Hello, Sweden!")

# Stäng och ta bort filen
temp_file.close
temp_file.unlink
``` 

Uppskrivning till ditt program blir så här:

```ruby
Hello, Sweden!
```

## Djupdykning

Historiskt sett var skapandet av temporära filer en nödvändighet för att tillåta data manipulering i situationer där minnet var begränsat. Nu används det som ett sätt att säkra dataintegritet eller för att dela data mellan olika program.

Alternativt kan du också använda `StringIO`-objekt om du bara behöver en tillfällig lagringsplats och inte en faktisk fil. Men, kom ihåg att dessa objekt endast existerar i minnet, inte på disk.

Implementeringsdetaljer att komma ihåg inkluderar att `Tempfile.new` skapar filen i ditt systems temp-katalog. Därför får den automatiskt rättigheterna för det katalogen. Dessutom om du glömmer att ta bort filen kommer Ruby att göra det när objektet blir garbage collected.

## Se även

För att få ytterligare information och kunskap, se följande länkar:

1. Officiell dokumentation för Tempfile-klass: https://ruby-doc.org/stdlib/libdoc/tempfile/rdoc/Tempfile.html
2. Ruby IO Guide, inklusive StringIO-detaljer: https://ruby-doc.org/core/io.html