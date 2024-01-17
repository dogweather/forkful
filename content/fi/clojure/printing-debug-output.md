---
title:                "Tulostamalla vianetsintätulostetta"
html_title:           "Clojure: Tulostamalla vianetsintätulostetta"
simple_title:         "Tulostamalla vianetsintätulostetta"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Mikä on ja miksi? 
Debug-tulostuksen printtaaminen tarkoittaa ohjelmakoodin suorituksen aikana tietojen tulostamista terminaaliin. Tämä auttaa ohjelmoijaa hahmottamaan ohjelman suoritusta ja löytämään mahdollisia virheitä tai bugeja. 

# Miten tehdä: 
```Clojure
(def x 5) 
(print x)
```
Tässä esimerkissä määritellään muuttuja x arvolla 5 ja tulostetaan se terminaaliin. 
```Clojure
(defn addition [a b]
  (print "Performing addition...")
  (println (+ a b)))
```
Tässä esimerkissä määritellään funktio, joka suorittaa yhteenlaskun ja tulostaa välivaiheessa tiedon sen suorittamisesta. 

# Syvällisempi sukellus: 
Debug-tulostaminen on ollut käytössä ohjelmoinnissa jo pitkään ja se on yksi tapa helpottaa ohjelmien testaamista ja kehittämistä. Vaihtoehtoisesti voit myös käyttää debuggaus-työkaluja, kuten debugger-ohjelmia, jotka antavat tarkempaa tietoa ohjelman suorituksesta. Implementaation osalta, print-funktio käyttää standardia "OutputStream"-oliota, joka on yhteydessä terminaaliin. 

# Katso myös: 
- [Clojure Debugging Techniques](https://blog.klipse.tech/clojure/2017/02/01/clojure-debug-techique.html)
- [Clojure Built-In Functions](https://clojuredocs.org/clojure.core/print)