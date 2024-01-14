---
title:                "Elixir: Regular expressionien käyttö"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttäisit säännöllisiä lausekkeita Elixir-ohjelmoinnissa?

Säännölliset lausekkeet ovat voimakas työkalu, jota Elixir-ohjelmoijat voivat käyttää datan käsittelyyn ja muokkaamiseen. Niitä voidaan käyttää esimerkiksi merkkijonojen hakuun ja korvaamiseen, datan validointiin ja parsimiseen. Säännöllisillä lausekkeilla voi olla suuri vaikutus koodin tehokkuuteen ja luettavuuteen.

## Miten säännöllisiä lausekkeita käytetään Elixirissä?

Käytämme säännöllisiä lausekkeita "Regex" -moduulin kautta, joka sisältää valmiita toimintoja säännöllisten lausekkeiden käsittelyyn. Voimme myös luoda omia säännöllisiä lausekkeita "Regex" -kääreellä.

```Elixir
Regex.match?(~/hello/, "hello") # Palauttaa true
Regex.replace(~r/world/, "hello world", "elixir") # Palauttaa "hello elixir"
```

## Syvällisempi sukellus säännöllisten lausekkeiden käyttöön

Säännöllisiä lausekkeita voidaan käyttää monella eri tavalla Elixirissä. Yksi hyödyllisimistä tavoista on merkkijonojen haku ja korvaaminen. Esimerkiksi voimme käyttää säännöllisiä lausekkeita tarkistaaksemme, onko tietty sana tai merkkijono annetussa tekstissä.

```Elixir
Regex.match?(~r/elixir/, "I love Elixir!") # Palauttaa true
```

Voimme myös käyttää säännöllisiä lausekkeita datan parsimiseen ja validointiin. Esimerkiksi voimme tarkistaa, onko annettu merkkijono kelvollinen sähköpostiosoite käyttämällä säännöllistä lauseketta.

```Elixir
Regex.match?(~r/@/, "example@email.com") # Palauttaa true
```

Lisäksi voimme käyttää säännöllisiä lausekkeita tehokkaasti tiedonetsintään. Voimme esimerkiksi etsiä kaikki numerot annetusta merkkijonosta käyttämällä säännöllistä lauseketta.

```Elixir
Regex.scan(~r/\d+/, "There are 5 apples and 10 oranges") # Palauttaa ["5", "10"]
```

## Katso myös

- [Elixir-kirjaston "Regex" virallinen dokumentaatio](https://hexdocs.pm/elixir/Regex.html)
- [Säännöllisten lausekkeiden perusteet Elixirissä](https://www.tutorialspoint.com/elixir/elixir_regular_expressions.htm)
- [Säännöllisten lausekkeiden hyödyntäminen Elixiriin liittyvissä tehtävissä](https://medium.com/@josevalim/matching-hl7-messages-with-elixir-7b220559f5e6)