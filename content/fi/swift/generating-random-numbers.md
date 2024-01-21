---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:50:10.665475-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Arpajaiset verkossa, pelien sattumanvaraisuus, testidata – kaikki vaativat satunnaislukuja. Ohjelmoijat käyttävät niitä simuloimaan arvaamattomuutta ja testaamaan skenaarioita.

## How to: (Kuinka tehdä:)
Generoi satunnaisluku Swiftissä näin:

```Swift
import Foundation

// Satunnainen kokonaisluku välillä 0-99
let randomNumber = Int.random(in: 0..<100)
print(randomNumber)

// Satunnainen liukuluku välillä 0-1
let randomFloat = Float.random(in: 0..<1)
print(randomFloat)
```

Esimerkkituloste:

```
42
0.123456
```

## Deep Dive (Syväsukellus)
Satunnaislukujen generoiminen on monimutkaisempaa kuin miltä se näyttää. Historiallisesti käytettiin matemaattisia kaavoja (kuten lineaarinen kongruenssi menetelmä), mutta ne eivät ole aidosti satunnaisia. Nykyisin suositaan kryptografisesti turvallisia menetelmiä, jotka tuottavat ennustamattomampia lukuja.

Swiftissä `Int.random(in:)` ja `Float.random(in:)` käyttävät ARC4-pohjaista pseudosatunnaisgeneraattoria, joka vastaa tarvetta useimmissa tilanteissa. Kryptografisesti turvallisempaan tarpeeseen käytetään `SecRandomCopyBytes` funktiota Darwinin turvallisuusrajapinnasta.

## See Also (Katso myös)
- Swiftin virallinen dokumentaatio: [Swift Documentation](https://swift.org/documentation/)