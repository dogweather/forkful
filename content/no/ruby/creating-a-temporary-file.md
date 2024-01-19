---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Opprettelse av en midlertidig fil er prosessen med å lage en fil for kortvarig bruk. Vi gjør det for å lagre data midlertidig, eller når det er for riskabelt å jobbe med originalfilen.

## Hvordan gjøre det: 

I Ruby er det flere måter å opprette en midlertidig fil på, men den mest grunnleggende er med klassen `Tempfile`. Her er en grunnleggende bruk:

```Ruby
require 'tempfile'

temp = Tempfile.new('temp1')

begin
  temp << 'Hello world!'
  temp.rewind
  puts temp.read
ensure
  temp.close
  temp.unlink
end
```

Her er utdata:

```Terminal
Hello world!
```

## Dypdykk

Opprettelsen av midlertidige filer har vært en del av programmeringsspråk siden de tidligste dagene. Det spiller en viktig rolle i håndteringen av store datamengder som kan overskride minnekapasiteten.

Når det gjelder alternativer til `Tempfile`, kan man også opprette midlertidige filer med `Dir::Mktmpdir`, som lar deg opprette en midlertidig katalog i stedet for bare en fil.

Når det kommer til implementering, har `Tempfile` en innebygget metode kalt `#unlink`. Denne metoden vil slette den midlertidige filen umiddelbart etter bruk, noe som bidrar til å forhindre avfall av diskplass.

## Se også

For mer detaljert informasjon om midlertidige filer i Ruby, kan du besøke de offisielle Ruby-dokumentene: [Tempfile](https://ruby-doc.org/stdlib-3.1.1/libdoc/tempfile/rdoc/Tempfile.html)

For å lære mer om alternativene, kan du lese den offisielle dokumentasjonen for `Dir::Mktmpdir`: [Dir](https://ruby-doc.org/core-3.1.1/Dir.html#method-c-mktmpdir)