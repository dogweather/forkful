---
title:    "Elixir: Merkkijonon muuntaminen pienaakkosiksi"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme muuttaa merkkijonon pieniksi kirjaimiksi Elixir-ohjelmoinnissa? Yksi yleinen syy voisi olla tietojen yhtenäistäminen. Jos työskentelet esimerkiksi käyttäjien syöttämien tietojen kanssa, haluat todennäköisesti varmistaa, että kaikki syötteet ovat samassa muodossa, jotta niitä on helpompi käsitellä ja vertailla.

## Kuinka tehdä

Elixirissä on sisäänrakennettu toiminto `String.downcase`, joka muuttaa annetun merkkijonon pieniksi kirjaimiksi. Voit käyttää sitä yksinkertaisesti antamalla sille merkkijonon parametrina ja tallentamalla palautetun arvon muuttujaan.

```Elixir
input = "ELIXIR OHJELMOINTI"
output = String.downcase(input)
IO.puts output
```

Tämä koodi tulostaa "elixir ohjelmointi" konsoliin. Tässä esimerkissä käytämme `IO.puts` -toimintoa tulostamaan tekstin konsoliin, mutta voimme myös tallentaa palautetun arvon muuttujaan jatkokäsittelyä varten.

## Syvemmälle

Vaikka `String.downcase` on kätevä työkalu yksinkertaisten merkkijonojen muuttamiseen pieniksi kirjaimiksi, sen toiminta perustuu Unicode-standardiin. Tämä tarkoittaa, että se osaa käsitellä myös monimutkaisempia kieliä tai kirjaimia, jotka eivät ole vain aakkosia. Esimerkiksi jos annamme sille merkkijonon, jossa on akut, se muuttaa nekin pieniksi kirjaimiksi oikein.

`String.downcase` toimii myös sujuvasti koko merkkijonon kanssa, eikä vain ensimmäisen kirjaimen kanssa, kuten jotkut muut kielet mahdollisesti tekevät.

## Katso myös

- [Elixirin virallinen dokumentaatio String-moduulille](https://hexdocs.pm/elixir/String.html)
- [Blogipostaus merkkijonon muuttamisesta isojen kirjainten ja pienten kirjainten välillä Elixirissä](https://medium.com/@kasunpd/elixir-change-the-case-of-all-letters-in-a-string-feee68d8f697) (englanniksi)