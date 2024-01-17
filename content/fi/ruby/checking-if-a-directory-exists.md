---
title:                "Kansion olemassaolon tarkistaminen"
html_title:           "Ruby: Kansion olemassaolon tarkistaminen"
simple_title:         "Kansion olemassaolon tarkistaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tarkistamalla, onko hakemisto olemassa, selvitetään, onko tietylle polulle tallennettu tiedostoja tai kansioita. Tämä on tärkeää, jotta voidaan varmistaa, että halutut tiedostot ovat oikeassa paikassa ja koodin suoritus ei epäonnistu. Tämä on yleinen käytäntö ohjelmointimaailmassa, joka auttaa välttämään virheitä ja suorituskykyongelmia.

## Miten:

```Ruby
if Dir.exist?("hakemiston_nimi")
  puts "Hakemisto on olemassa!"
else
  puts "Hakemistoa ei löytynyt."
end
```

Yllä olevassa koodissa käytämme Ruby'n sisäänrakennettua `Dir.exist?`-metodia tarkistaaksemme, onko annetulla polulla oleva hakemisto olemassa. Jos hakemisto löytyy, tulostamme viestin "Hakemisto on olemassa!", muuten tulostamme "Hakemistoa ei löytynyt."

## Syvempi sukellus:

Tarkistamalla hakemiston olemassaolon on pitkät juuret historiassa. Aikaisemmin ohjelmoijat joutuivat käyttämään monimutkaisempia menetelmiä, kuten luomaan tiedostoja tai muuttujia tarkistaakseen hakemistoja. Onneksi Ruby'ssa on nyt sisäänrakennettu metodi, joka tekee tämän tehtävän paljon helpommaksi.

On myös muita tapoja tarkistaa hakemiston olemassaolo, kuten käyttämällä väärien käsittelyä, mikä suorittaa tarkistuksen automaattisesti virheen sattuessa. Tämä voi olla hyödyllistä, jos haluat välttää tarpeettomia tarkistuksia koodissasi.

Teknisten yksityiskohtien osalta Dir.exist? -metodi käyttää `File.directory?`-metodia tarkistaakseen, onko kyseessä hakemisto. Tämä on hyvä pitää mielessä, jos haluat käyttää samanlaisia tarkistuksia tiedostojen tai muiden kohteiden olemassaolosta.

## Katso myös:

Voit löytää lisätietoa Dir.exist? -metodista Ruby'n virallisesta dokumentaatiosta osoitteessa https://ruby-doc.org/core-3.0.0/Dir.html#method-c-exist-3F. Voit myös tutkia muita hyödyllisiä Ruby-metodeja ja -toimintoja tämän sivun kautta.