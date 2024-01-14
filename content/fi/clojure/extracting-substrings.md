---
title:    "Clojure: Alimerkkijonojen erottelu"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Joissakin tapauksissa ohjelmoijan on tarpeen erottaa merkkijonosta tietyt osat, jotka vastaavat tiettyä kaavaa tai kriteeriä. Tämä voi olla esimerkiksi hakeminen tietokannassa olevista nimistä, joissa on tietyt kirjaimet tietyssä järjestyksessä. Tätä kutsutaan substringien erottamiseksi ja se on erittäin hyödyllinen ja kätevä tapa käsitellä dataa.

## Kuinka tehdä

Voit erottaa substringin Clojure-kielessä käyttämällä funktiota "subs". Esimerkiksi, jos haluamme erottaa merkkijonosta "Hello World" vain sanan "World", voimme käyttää seuraavaa koodia:

```Clojure
(subs "Hello World" 6)
```

Tämä tulostaisi "World", koska annamme sille aloituskohdan 6, joka vastaa "W" -kirjainta. Voimme myös määrittää lopetuskohdan, jos haluamme erottaa useiden merkkien substringin. Katso alla oleva esimerkki:

```Clojure
(subs "Hello World" 0 5)
```

Tämä tulostaa "Hello", koska aloituskohdaksi annetaan 0 ja lopetuskohdaksi 5, mikä vastaa merkkien "H", "e", "l", "l" ja "o" välillä olevaa substringia.

## Syvä sukellus

Clojuressa on myös mahdollista erottaa substringin määrätyn kriteerin perusteella. Käyttämällä funktiota "re-find", voimme määrittää säännöllisen lausekkeen, joka vastaa etsittävää tekstiä. Esimerkiksi, jos haluamme erottaa kaikki merkkijonosta "aaabbbccc" vain puhtaat kirjaimet "a", voimme käyttää seuraavaa koodia:

```Clojure
(re-find #"a*" "aaabbbccc")
```

Tämä tulostaisi "aaa", koska *-merkki vastaa nollasta äärettömään kappaleeseen edellä olevaa kirjainta. Tämä on vain yksi esimerkki säännöllisen lausekkeen käytöstä, joka voi olla erittäin hyödyllinen monimutkaisempien substringien etsimisessä tietyistä merkkijonoista.

## Katso myös

- Clojuren viralliset nettisivut: https://clojure.org/
- Opas säännöllisten lausekkeiden käyttöön Clojurella: https://clojure.org/guides/learn/regular_regex_tokens