---
title:    "Clojure: Väliaikaistiedoston luominen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Joskus Clojure-ohjelmoijana saatat joutua luomaan väliaikaisia tiedostoja. Tämä voi johtua esimerkiksi tarpeesta tallentaa väliaikaisia tietoja tai jakaa tietoja eri prosessien välillä. Tällaisessa tilanteessa voi olla hyödyllistä tietää, miten luoda väliaikainen tiedosto Clojuren avulla.

## Miten

Clojurella on sisäänrakennettu toiminto `with-open`, joka tarjoaa mahdollisuuden luoda ja käsitellä väliaikaisia tiedostoja. Alla on yksinkertainen esimerkki, joka luo väliaikaisen tekstitiedoston, kirjoittaa siihen tekstin ja lukee sen sisällön.

```Clojure
(with-open [file (io/file "tmp_file.txt")]
  (spit file "Tämä on väliaikainen tiedosto.")
  (println (slurp file)))
```

Tämä koodi tuottaa seuraavan tulosteen:

```
Tämä on väliaikainen tiedosto.
```

Voit myös käyttää Clojuren `temp-dir`-funktiota luomaan väliaikaisen hakemiston, johon voit tallentaa tiedostoja.

## Syventyminen

Kun luo väliaikaisia tiedostoja, on tärkeää muistaa, että ne eivät automaattisesti poistu, kun ohjelma päättyy. Sinun täytyy siis itse huolehtia niiden poistamisesta. Clojurella on kuitenkin `delete-file`-funktio, jolla voit poistaa tiedostoja.

Voit myös muokata `with-open` -funktion esimerkkiä niin, että tiedosto poistetaan automaattisesti, kun se ei ole enää tarpeellinen. Tämä onnistuu käyttämällä `let`-lauseketta ja `when`-ehtolauseketta.

```Clojure
(let [tmp-file (io/file "tmp_file.txt")]
  (with-open [file tmp-file]
    (spit file "Tämä on väliaikainen tiedosto.")
    (when (exists? tmp-file)
      (delete-file tmp-file))))
```

Tällöin tiedosto poistetaan automaattisesti, kun koodi suoritetaan loppuun.

## Katso myös

- [Clojure Dokumentaatio: io.nippy](https://clojuredocs.org/clojure.java.io#io/fn)
- [Create temporary files in Java](https://www.baeldung.com/java-temporary-files)
- [Clojure Cookbook: Creating temporary files](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/06_files-io/6-02_creating-temporary-files.asciidoc)