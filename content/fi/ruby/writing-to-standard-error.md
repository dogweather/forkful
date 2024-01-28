---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (stderr) on Rubyssa tapa raportoida virheet ja varoitukset. Ohjelmoijat kirjoittavat stderr:ään, koska se auttaa erottamaan normaalit logiviestit todellisista ongelmista.

## How to:
Kirjoittaminen stderr:ään on yksinkertaista. Käytä `$stderr.puts` tai `warn`-metodia.

```Ruby
# Esimerkki stderr-kirjoittamisesta
$stderr.puts "Tämä on virheilmoitus"
warn "Tämä on toinen tapa kirjoittaa virheilmoituksiin"
```

Tulostuu:
```
Tämä on virheilmoitus
Tämä on toinen tapa kirjoittaa virheilmoituksiin
```

## Deep Dive
`stderr` on UNIX-järjestelmistä peräisin oleva perinne, erottaa standarditulosteen (stdout) virheilmoituksista. Rubyssa voit myös ohjata `stderr`-virtaa tiedostoon tai toiseen kohteeseen. `warn` on metodi, joka oletusarvoisesti kirjoittaa stderr:iin ja on käytännöllinen, koska se voidaan konfiguroida ohittavaksi viestit asettamalla `$VERBOSE = nil`.

```Ruby
# Ohjaa stderr tiedostoon
File.open('virheet.log', 'w') do |file|
  $stderr.reopen(file)
  $stderr.puts "Tämä virhe menee tiedostoon."
end
```

## See Also
- Ruby-dokumentaatio virheiden käsittelystä: [https://ruby-doc.org/core-2.7.0/IO.html#method-c-new-label-IO+Open+Mode](https://ruby-doc.org/core-2.7.0/IO.html#method-c-new-label-IO+Open+Mode)
- UNIX-ohjeet ohjaukseen: [https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
