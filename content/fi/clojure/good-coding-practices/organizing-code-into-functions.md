---
date: 2024-01-26 01:10:14.589334-07:00
description: "Miten: Clojure-funktiot m\xE4\xE4ritell\xE4\xE4n k\xE4ytt\xE4en `defn`,\
  \ sen j\xE4lkeen tulee nimi, parametrit ja runko. T\xE4ss\xE4 on nopea esimerkki."
lastmod: '2024-03-13T22:44:56.189738-06:00'
model: gpt-4-1106-preview
summary: "Clojure-funktiot m\xE4\xE4ritell\xE4\xE4n k\xE4ytt\xE4en `defn`, sen j\xE4\
  lkeen tulee nimi, parametrit ja runko."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Miten:
Clojure-funktiot määritellään käyttäen `defn`, sen jälkeen tulee nimi, parametrit ja runko. Tässä on nopea esimerkki.

```Clojure
(defn greet [name]
  (str "Hello, " name "!"))

(greet "Alex") ; => "Hello, Alex!"
```

Kuvitellaan nyt, että haluamme laskea suorakulmion pinta-alan. Sen sijaan, että sotkisimme kaiken yhteen, erotamme sen kahdeksi funktioksi:

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "The area is:" (area length width)))

(print-area 3 4) ; => The area is: 12
```

## Syväluotaus
Aikojen alussa koodaajat vain mukauttivat kaiken logiikkansa yhteen lohkoon. Se oli ruma näky. Sitten rakennoitu ohjelmointi tuli kuvioihin, ja funktioista tuli juttu. Clojuressa jokainen funktio on ensiluokkainen – voit heittää niitä ympäriinsä kuten mitä tahansa muuta arvoa.

Vaihtoehtoja? Jotkut saattaisivat kikkailla multimetodeilla tai korkeamman asteen funktioilla, mutta nuo ovat vain mausteita funktiopadassa.

Kaikki funktioiden yksityiskohdissa: ne ovat muuttumattomia Clojuressa, mikä tekee sivuvaikutusten sekasorrosta epätodennäköisempää. Ne nojautuvat voimakkaasti rekursioon tyypillisten silmukoiden sijaan, mikä istuu hyvin yksiin kielen toiminnallisten paradigmojen kanssa.

## Katso Myös
- Clojuren oma opas: https://clojure.org/guides/learn/functions
- Funktionaalisen Ohjelmoinnin Alkeet: https://www.braveclojure.com/core-functions-in-depth/
- Rich Hickeyn Puheenvuorot: https://changelog.com/posts/rich-hickeys-greatest-hits - näkemyksiä Clojuren filosofiasta.
