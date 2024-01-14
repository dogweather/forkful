---
title:    "Ruby: Tarkistetaan, onko kansio olemassa"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Miksi tarkistaa, onko hakemisto olemassa?

Usein ohjelmoinnissa joudumme käsittelemään erilaisia tiedostoja ja hakemistoja. Joskus tarvitsemme tietoa siitä, onko jokin hakemisto olemassa ennen kuin voimme suorittaa tiettyjä toimintoja. Tässä blogikirjoituksessa käymme läpi, miten voit tarkistaa, onko hakemisto olemassa käyttämällä Ruby-ohjelmointikieltä.

## Kuinka tarkistaa, onko hakemisto olemassa?

Rubyssa on helppo tarkistaa, onko hakemisto olemassa. Siihen käytämme `File`-luokan `directory?`-metodia. Voit käyttää sitä seuraavalla tavalla:

```Ruby
if File.directory?("/home/käyttäjä/hakemisto")
  puts "Hakemisto on olemassa."
else
  puts "Hakemistoa ei löytynyt."
end
```

Yllä olevassa esimerkissä tarkistamme, onko "/home/käyttäjä/hakemisto" -hakemisto olemassa. Jos se on, tulostetaan "Hakemisto on olemassa". Jos se ei ole, tulostetaan "Hakemistoa ei löytynyt".

## Syvempi sukellus

Periaatteessa `File.directory?`-metodi tarkistaa annetun polun ja palauttaa `true` tai `false` sen mukaan, onko kyseessä hakemisto vai ei. Tästä voimme päätellä, että emme voi käyttää tätä metodia tarkistaaksemme, onko tiedosto olemassa. Tähän tarkoitukseen meidän tulisi käyttää `File`-luokan `exist?`-metodia. Se toimii samalla tavalla kuin `directory?`-metodi, mutta tarkistaa, onko kyseinen polku olemassa oleva tiedosto. Esimerkiksi:

```Ruby
if File.exist?("/home/käyttäjä/tiedosto.txt")
  puts "Tiedosto on olemassa."
else
  puts "Tiedostoa ei löytynyt."
end
```

Voit myös tarkistaa, onko tiedosto tai hakemisto olemassa käyttämällä `Dir`-luokan `exist?`-metodia. Se toimii samalla tavalla kuin `File.exist?`-metodi. Esimerkiksi:

```Ruby
if Dir.exist?("/home/käyttäjä/hakemisto")
  puts "Hakemisto on olemassa."
else
  puts "Hakemistoa ei löytynyt."
end
```

On myös hyvä huomata, että `File.directory?`- ja `Dir.exist?`-metodit tarkistavat, onko kyseessä juuri hakemisto, eivät alihakemistoja. Tämä tarkoittaa sitä, että jos haluat tarkistaa, onko jokin alihakemisto olemassa, sinun tulisi käyttää `Dir.exist?`-metodia tarkistaaksesi ensin, onko ylähakemisto olemassa ja sen jälkeen käyttää `File.directory?`-metodia tarkistaaksesi, onko alihakemisto olemassa.

## Katso myös

- Ruby File-luokka: https://ruby-doc.org/core-2.6.3/File.html
- Ruby Dir-luokka: https://ruby-doc.org/core-2.6.3/Dir.html
- Ruby Docs: https://ruby-doc.org/