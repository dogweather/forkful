---
title:                "Clojure: Lukeminen komentoriviparametreista"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Miksi

Ihmiset kommunikoivat tietokoneohjelmien kanssa monin eri tavoin. Yksi kätevä tapa on lukea komentoriviparametreja. Tässä blogikirjoituksessa opimme, kuinka voimme lukea komentoriviparametreja Clojure-ohjelmassamme.

# Kuinka

```Clojure
(defn -main [& args]
  (println "Komentoriviparametreja oli " (count args))
  (if (empty? args)
    (println "Ei parametreja annettu")
    (doseq [arg args]
      (println "Saatu parametri: " arg))))
```

Koodirivit selitetään alla.

Ensimmäisellä rivillä luodaan funktio nimeltä `-main`, joka odottaa parametreja `& args` muodossa.

Toisella rivillä tulostetaan viesti komentoriviparametrien määrästä käyttämällä `count` funktiota ja `println` funktiota. Tämän jälkeen käytetään `if` lauseketta tarkistamaan, onko parametreja annettu vai ei.

Jos parametreja ei ole annettu, tulostetaan virheilmoitus "Ei parametreja annettu".

Jos parametreja on annettu, käytetään `doseq` funktiota käymään läpi kaikki parametrit ja tulostetaan jokainen niistä erikseen.

# Syvennä

Komentoriviparametrien lukeminen voi olla hyödyllistä esimerkiksi silloin, kun haluamme antaa ohjelmalle tietoa ennen sen suorittamista. Parametrit voivat sisältää esimerkiksi tiedostonimiä, argumentteja tai muita arvoja, joita ohjelma tarvitsee toimiakseen.

Komentoriviparametrien lukeminen voidaan tehdä myös käyttämällä Clojuren `ARGV` muuttujaa, joka sisältää listan kaikista parametreista. Tämä muuttuja on kuitenkin vain luku eikä sitä voi muuttaa.

# Katso myös

- [Official Clojure documentation for command line arguments](https://clojure.org/reference/repl_libraries)
- [ClojureDocs kokoelma-komentoriviparametreja käsitteleviä ohjeita](https://clojuredocs.org/clojure.core/ARGV)