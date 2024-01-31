---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä & Miksi? Testaus varmistaa koodin toimivuuden. Kehittäjät kirjoittavat testejä ennakoidakseen virheitä ja säilyttääkseen ohjelmiston laadun.

## How to:
Testien kirjoittaminen Clojuressa:

```Clojure
;; Add `clojure.test` to your namespace
(ns example.test
  (:require [clojure.test :refer :all]))

;; Define a test case
(deftest test-my-function
  (testing "Function output"
    (is (= 42 (my-function 40 2)))))

;; Run the tests
(run-tests)
```

Odotettu tulos:

```
Testing example.test

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Deep Dive
Syväsukellus: Clojuressa testien kirjoittaminen juontaa juurensa LISP:n ajoista, joissa koodikokeilu oli peruskäytäntö. Vaihtoehtoina on muun muassa generatiiviset testikirjastot, kuten `test.check`. Testien toteutuksessa käytetään usein `deftest` ja `is` makroja sekä testipuitteita, esimerkiksi `Midje` tai `Kaocha`.

## See Also
Kurkkaa myös:
- [Clojure Testing](https://clojure.github.io/clojure/clojure.test-api.html)
- [Introduction to Midje](https://github.com/marick/Midje/wiki)
- [Clojure generative testing with test.check](https://github.com/clojure/test.check)
