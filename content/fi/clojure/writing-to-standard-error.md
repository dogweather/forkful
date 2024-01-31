---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä & Miksi?
Kirjoittaminen standardivirheeseen tarkoittaa virheviestien lähettämistä erilliseen virhevojaan. Ohjelmoijat tekevät sen erottaakseen ohjelman normaalin tulosteen ja virhetiedot selkeästi toisistaan.

## How to:
Koodiesimerkkejä:
```Clojure
;; Kirjoittaminen standardivirheeseen
(.println System/err "Tässä on virheviesti.")

;; Sample Output
;; Standard error: Tässä on virheviesti.
```

Jos haluat ohjata virheviestit tiedostoon:
```Clojure
(with-open [wrtr (java.io.FileWriter. "virheloki.txt")]
  (.write wrtr "Tiedostoon kirjoitettu virheviesti.\n"))
```

## Deep Dive
Syväsukellus
Historiassa tulosteet ja virheet menivät usein samaan kohteeseen. UNIX-järjestelmissä erotettiin standardituloste (stdout) ja standardivirhe (stderr) paremman hallinnan takia. Clojuressa käytetään Javan `System/err` oliota virheiden kirjoittamiseen. Vaihtoehtoja on muitakin, kuten kirjastot, jotka tarjoavat lisäominaisuuksia, kuten loggausta. 

## See Also
Lisätietoa:
- [Clojure Documentation](https://clojure.org/guides/getting_started)
- [Clojure's error handling](https://clojure.org/guides/faq#error_handling)
- [Java's System class](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html)
