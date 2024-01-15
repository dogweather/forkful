---
title:                "Nedlasting av nettside"
html_title:           "Ruby: Nedlasting av nettside"
simple_title:         "Nedlasting av nettside"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å laste ned en nettside? Jo, kanskje du ønsker å lagre en kopi av en nettside for senere lesing uten å være koblet til internett, eller kanskje du ønsker å analysere koden eller innholdet på nettsiden.

## Hvordan gjøre det

For å laste ned en nettside i Ruby, kan du bruke biblioteket "open-uri" og metoden "open" for å åpne en URL. Deretter kan du bruke "read" metoden for å lese innholdet på nettsiden. Her er et eksempel som vil lagre innholdet på nettsiden "https://www.vg.no" i en variabel kalt "vg":

```Ruby
require 'open-uri'
vg = open("https://www.vg.no").read
```

Etter dette kan du gjøre hva du måtte ønske med innholdet på nettsiden, for eksempel analysere koden eller skrive den ut til konsollen. Ved å bruke metoden "puts" vil innholdet på nettsiden bli skrevet ut:

```Ruby
puts vg
```

Dette vil skrive ut alt innholdet på nettsiden til konsollen, slik at du kan lese det der. Du kan også lagre innholdet på nettsiden til en fil ved å bruke "open" og "write" metodene:

```Ruby
File.open("vg.html", 'w') do |file|
  file.write(vg)
end
```

Dette vil lagre innholdet på nettsiden i en fil kalt "vg.html". Du kan endre filnavnet og lagre innholdet på nettsiden til en fil med et passende navn.

## Dypdykk

Når du bruker "open-uri" biblioteket, kan du også legge til ekstra parametere som å spesifisere HTTP-headers, håndtere omadressering og autentisering. Dette kan være nyttig i mer komplekse situasjoner hvor du trenger å laste ned en nettside med spesifikke innstillinger. Du kan også velge å lagre innholdet på nettsiden i forskjellige formater, som for eksempel JSON eller CSV, ved å bruke passende metoder og libs.

## Se også

- Ruby's open-uri og File biblioteker: [https://ruby-doc.org/stdlib-2.6.3/libdoc/open-uri/rdoc/index.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/open-uri/rdoc/index.html)
  [https://ruby-doc.org/core-2.6.3/File.html](https://ruby-doc.org/core-2.6.3/File.html)
- En artikkel om å parse og manipulere nettsider i Ruby: [https://www.rubyguides.com/ruby-tutorial/scraping-websites/](https://www.rubyguides.com/ruby-tutorial/scraping-websites/)