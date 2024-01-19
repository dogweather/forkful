---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Artikkeli: Tulostamassa Debug-tulostetta ohjelmoinnissa Clojurella
 
## Mikä & Miksi?

Debug-tulostetta hyödyntämällä ohjelmoija pystyy syöttämään ja tulostamaan ohjelman suorittamisen aikana tietoja, jotka auttavat ohjelman virheiden jäljittämisessä ja korjaamisessa. Se on erittäin tärkeä osa ohjelmoinnissa tapahtuvaa ongelmanratkaisuprosessia.

## Miten:

Seuraavassa esimerkki Clojuren println-funktion käytöstä:

```Clojure
(defn debug-esimerkki []
  (let [x 5
        y 3]
    (println "Debug: x on" x ", y on" y)))
```

Kun ajamme tämän funktion, saamme seuraavanlaisen tulosteen:

```Clojure
Debug: x on 5 , y on 3
```

## Syväsukellus:

Historiallisesti ohjelmoijat ovat käyttäneet debug-tulostetta ohjelmisto-ongelmien löytämiseen ja ratkaisemiseen. Clojuren println-funktion lisäksi on olemassa monia muita työkaluja, kuten log4j ja tools.logging, jotka auttavat tuottamaan rikkaamman debug-information. 

Clojuren println-funktio kirjoittaa tiedot konsoliin ja suoritetaan arvojen laskennassa sijaitsevan sijainnin mukaan. Tämä voi olla hyödytön monisäikeisessä ympäristössä, jossa järjestystä ei voida taata.

## Katso myös:

- Clojure Programming Cookbook: https://www.packtpub.com/product/clojure-programming-cookbook/9781785885037
  Syvempi katsaus Clojuren debug-toimintoihin.
  
- Clojure for the Brave and True: https://www.braveclojure.com/
  Kookoskirja, joka tarjoaa yleiskatsauksen Clojuren eri osa-alueisiin, mukaan lukien debug-tulostaminen.

- Debugging with the Scientific Method: https://www.youtube.com/watch?v=FihU5JxmnBg
  Videoluento, joka käsittelee tieteellisen metodin soveltamista debuggaamisessa.
  
Boon Hellman – Ohjelmoinnin vapaa ammattilainen.