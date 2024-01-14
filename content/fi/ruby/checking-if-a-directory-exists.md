---
title:                "Ruby: Tarkistaako hakemisto on olemassa"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi voit haluta tarkistaa, onko kansio olemassa. Yksi yleisin syy on, jos haluat varmistaa, että tietty kansio on luotu ennen kuin suoritat tietyn toiminnon.

## Kuinka tarkistaa kansion olemassaolo

Rubyssa voit helposti tarkistaa kansion olemassaolon käyttämällä `File.exist?` -metodia. Tämä metodi ottaa parametrina kansion polun ja palauttaa totuusarvon sen perusteella, löytyykö kyseinen kansio vai ei. Tässä on yksinkertainen esimerkki:

```Ruby
if File.exist?("kansio/nimi")
  puts "Kansio on olemassa"
else
  puts "Kansiota ei löydy"
end
```

Jos `File.exist?` palauttaa `true`, tulostetaan "Kansio on olemassa". Muussa tapauksessa tulostetaan "Kansiota ei löydy".

## Syvempi sukellus

`File.exist?` -metodi todella tarkistaa, onko tiedosto tai kansio nimeltään samanlainen kuin argumentti. Jos haluat tarkistaa, että kansio on olemassa, mutta sinulla ei ole tarkkaa nimeä, voit käyttää `Dir.exist?` -metodia. Tämä metodi tarkistaa, onko olemassa olevat objektit kansiossa nimetty saman yleisen kaavan mukaan. Esimerkiksi: `Dir.exist?("kansio/*")` palauttaa `true` jos kansiossa on vähintään yksi tiedosto tai kansio.

Kun tarkistat kansion olemassaolon, voit myös käyttää `begin/rescue` -rakennetta käsitelläksesi mahdollisia virheitä. Tämä auttaa välttämään ohjelman kaatumisen, jos esimerkiksi kansio on poistettu samalla kun yrität tarkistaa sen olemassaolon. Tässä on esimerkki:

```Ruby
begin
  if Dir.exist?("kansio/*")
    puts "Jokin kansiossa on olemassa"
  else
    puts "Kansiota ei löydy"
  end
rescue
  puts "Virhe tarkistaessa kansion olemassaoloa"
end
```

## Katso myös

- Rubyn `File`-luokan dokumentaatio: https://ruby-doc.org/core-2.7.2/File.html
- Rubyn `Dir`-luokan dokumentaatio: https://ruby-doc.org/core-2.7.2/Dir.html
- Rubyn virallinen opas: https://www.ruby-lang.org/fi/documentation/