---
date: 2024-01-20 17:36:19.612974-07:00
description: "How to: Clojure k\xE4ytt\xE4\xE4 Java-kirjastoja p\xE4iv\xE4m\xE4\xE4\
  rien k\xE4sittelyyn ja formaattiin. T\xE4ss\xE4 on esimerkki."
lastmod: '2024-03-13T22:44:56.195716-06:00'
model: gpt-4-1106-preview
summary: "Clojure k\xE4ytt\xE4\xE4 Java-kirjastoja p\xE4iv\xE4m\xE4\xE4rien k\xE4\
  sittelyyn ja formaattiin."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to:
Clojure käyttää Java-kirjastoja päivämäärien käsittelyyn ja formaattiin. Tässä on esimerkki:

```clojure
(import '(java.text SimpleDateFormat)
         '(java.util Date))

(defn format-date [date pattern]
  (let [date-format (SimpleDateFormat. pattern)]
    (.format date-format date)))

(println (format-date (Date.) "dd.MM.yyyy HH:mm:ss"))
;; Esimerkki tuloste: "21.03.2023 14:56:07"
```

## Deep Dive
Päivämäärän merkkijonoksi muuttaminen on tärkeää, jotta se voidaan ilmaista ymmärrettävästi ihmisille. Historiallisesti, tämä on ollut osa ohjelmointia Java-kirjastojen ansiosta, jota Clojure suoraan hyödyntää.

Vaihtoehtoisesti, Clojure-kirjasto `clj-time` tarjoaa rikkaamman API:n päivämäärille ennen JDK 8:a, mutta nykyisin useimmissa tapauksissa riittää `java.time` -kirjasto, joka tuli osaksi Javaa versiossa 8. Tässä esimerkki `java.time` käyttämisestä:

```clojure
(import '(java.time LocalDateTime)
         '(java.time.format DateTimeFormatter))

(defn format-date-java-time [datetime pattern]
  (let [formatter (DateTimeFormatter/ofPattern pattern)]
    (.format datetime formatter)))

(println (format-date-java-time (LocalDateTime/now) "dd.MM.yyyy HH:mm:ss"))
;; Esimerkki tuloste: "21.03.2023 14:59:23"
```

Muutos tulee toteen Java metodin `.format` avulla, jolle annetaan formaatti `DateTimeFormatter` tai `SimpleDateFormat` luokasta. Joustavuus formaatin suhteen tarkoittaa, että voit määrittää päivämäärän ulkoasun tarkasti tarpeidesi mukaan.

## See Also
1. [Clojure Docs](https://clojuredocs.org/)
2. [Java SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
3. [Java DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
4. [clj-time GitHub](https://github.com/clj-time/clj-time)
