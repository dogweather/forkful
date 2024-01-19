---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Javascript: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Hakemiston olemassaolon tarkistaminen tarkoittaa sitä, että ohjelmoija tarkastaa, onko tietty hakemisto jo olemassa tiedostojärjestelmässä. Tätä tehdään hakemiston kahden kertaisen luomisen välttämiseksi ja virheiden ehkäisemiseksi.

## Kuinka:

Rubyssa voit tarkistaa, onko hakemisto olemassa, käyttämällä Dir.exist? -metodia. 

```Ruby
if Dir.exist?('/polku/hakemisto')
  puts "Hakemisto on olemassa."
else
  puts "Hakemisto ei ole olemassa."
end
```
Tämän koodinpätkän tuloste olisi:

```
Hakemisto on olemassa.
```
tai

```
Hakemisto ei ole olemassa.
```
riippuen siitä, onko hakemisto “/polku/hakemisto” olemassa.

## Syvempi tarkastelu:

Dir.exist? -metodi on ollut olemassa jo pitkään ja on Rubyn standardikirjaston osa. Se on suoraviivainen ja helppo tapa tarkistaa, onko hakemisto olemassa.

Vaihtoehtoisesti voit käyttää File.directory? -metodia, mutta se ei paljasta, onko tiedostopolku olemassa, vaan sen, onko kyseinen polku hakemisto.

Historiallisesti Rubyssa oli tapana käydä läpi tiedostojärjestelmä hakemistojen löytämiseksi. Moderni Ruby mahdollistaa hakemistojen olemassaolon tarkistamisen suoraviivaisella ja intuitiivisella tavalla.

## Lisätietoja:

- Ruby-doc.org | Dir.exist? metodi: [Linkki](https://ruby-doc.org/core-2.6.5/Dir.html#method-c-exist-3F)
- Ruby-doc.org | File.directory? metodi: [Linkki](https://ruby-doc.org/core-2.6.5/File.html#method-c-directory-3F)