---
title:                "Skriva till standardfel"
date:                  2024-01-19
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standardfel (stderr) handlar om att separera programfel och diagnostisk output från vanlig output (stdout). Det underlättar felsökning och gör att felmeddelanden kan hanteras separat.

## How to:
I Ruby kan du skriva till stderr med `STDERR.puts` eller `$stderr.puts`. Här är exempel:

```Ruby
puts 'Det här är normal output.'
STDERR.puts 'Det här är ett felmeddelande.'
```

När du kör, får du något som:

```
Det här är normal output.
Det här är ett felmeddelande.
```

Stdout och stderr kan omdirigeras separat i terminalen.

## Deep Dive
Fram till mitten av 70-talet, då konceptet med standard streams introducerades i Unix, var outputhantering mer inkonsekvent. Stderr erbjuder en dedikerad stream så att felmeddelanden inte blandas med vanlig output. Alternativ till `STDERR.puts` inkluderar `warn` för att ge en varning, eller att använda `raise` för att kasta ett undantag. Bakom kulisserna använder Ruby globala variabler `$stdout` och `$stderr` som motsvarar dessa streams.

## See Also
- Ruby's IO class documentation: [IO - Ruby-Doc.org](https://ruby-doc.org/core/IO.html)
- En mer genomgripande guide till standard streams: [Wikipedia - Standard streams](https://en.wikipedia.org/wiki/Standard_streams)
- Ruby's Kernel Module: [Kernel - Ruby-Doc.org](https://ruby-doc.org/core/Kernel.html)
