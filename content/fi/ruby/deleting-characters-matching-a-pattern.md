---
title:    "Ruby: Palamisen vastaisten merkkien poistaminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Tämä voi olla hyödyllistä, jos esimerkiksi haluat puhdistaa käyttäjän syöttämissä tiedoissa olevat turhat merkinnät tai haluat suodattaa haluttuja merkkejä pois tekstiriviltä.

## Miten

Voit poistaa merkkejä, jotka vastaavat tiettyä kaavaa Rubyssa käyttämällä `gsub`-metodia, joka korvaa liittymäkohtaisesti kaikki halutut merkit halutuilla merkeillä. Tämän voi tehdä seuraavalla tavalla:

```Ruby
string = "Tervetuloa Rubyyn!"
new_string = string.gsub(/[aeiou]/, "")
puts new_string # Trvl Rbyyn!
```

Tässä esimerkissä kaikki vokaalit korvataan tyhjällä merkkijonolla, jolloin tuloksena olevassa uudessa merkkijonossa ei ole enää vokaaleita.

## Syvällisempi sukellus

`gsub`-metodin ensimmäinen argumentti on regex-kuvio, joka määrittää, mitkä merkit haluat poistaa. Voit käyttää tätä hyväksesi tekemällä monimutkaisempia kaavoja, kuten poistamalla vain tietyn pituiset merkkijonot tai tiettyjen merkkien yhdistelmät. Voit myös käyttää `gsub!`-metodia, joka muuttaa alkuperäistä merkkijonoa sen sijaan, että palauttaisi uuden merkkijonon.

## Katso myös

- [RegExr](https://regexr.com/) - Verkkosivusto, joka auttaa sinua luomaan ja testaamaan regex-kaavoja.
- [Ruby Regex -opas](https://www.rubyguides.com/2015/06/ruby-regex/?fbclid=IwAR2_ylrq76ACyDwIVsMwL3y-Dg6E-V6_Fa85w8umzOPV0Q6uoh7sql5UcGs) - Kattava opas regexien käyttämiseen Rubyn kanssa.
- [Ruby String -dokumentaatio](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub) - Lisätietoa `gsub`-metodista ja sen eri vaihtoehdoista.