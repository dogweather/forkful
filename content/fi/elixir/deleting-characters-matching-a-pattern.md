---
title:    "Elixir: Puoliksi luodut merkit, jotka vastaavat kaavaa"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Poistaminen toistuvia merkkejä vastaavista malleista voi olla hyödyllistä, kun halutaan käsitellä tekstiä tai merkkijonoja tietyllä tavalla. Tämä voi parantaa suorituskykyä ja tehdä koodista helpommin ymmärrettävää.

## Kuinka tehdä

Tässä esimerkissä näytämme, kuinka poistaa kaikki numerot merkkijonosta käyttämällä Elixirin `String.replace/3` funktiota.

```Elixir
string = "Abc123Def456Ghi789"
only_letters = String.replace(string, ~r/\d/, "")
IO.puts only_letters

// Output: AbcDefGhi
```

Käytämme `~r`-muodostetta luomaan säännöllisen lausekkeen, joka vastaa kaikkia numeroita. Sitten annamme `String.replace` -funktiolle säännöllisen lausekkeen sekä tyhjän merkkijonon, joka poistaa kaikki vastaavat numerot. Lopuksi tulostamme uuden merkkijonon.

Tämä toimii myös, jos haluat poistaa tiettyjä merkkejä tai merkkijonoja. Esimerkiksi, jos haluat poistaa kaikki pilkut merkkijonosta, voit käyttää säännöllistä lauseketta `~r/,/` ja antaa tyhjän merkkijonon `String.replace` -funktiolle.

## Syväsukellus

Elixirin String-moduulilla on muitakin hyödyllisiä funktioita, kuten `String.split/3`, joka jakaa merkkijonon annetun säännöllisen lausekkeen perusteella. Voit myös käyttää `String.replace/4`-funktiota, jolloin voit määrittää montako vastaavaa merkkiä haluat korvata.

Elixirin säännöllisillä lausekkeilla on myös monia muita käyttötapoja, kuten merkkijonojen validointi ja tiedon hakeminen. Jos haluat oppia lisää säännöllisistä lausekkeista ja niiden käytöstä, suosittelemme tutustumaan Elixirin virallisiin dokumentaatioihin.

## Katso myös

- [Elixirin viralliset dokumentaatiot säännöllisistä lausekkeista](https://hexdocs.pm/elixir/Regex.html)
- [Regex101 - verkkotyökalu säännöllisten lausekkeiden testaamiseen](https://regex101.com/r/AcBkgC/1)