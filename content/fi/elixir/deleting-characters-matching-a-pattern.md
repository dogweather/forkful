---
title:                "Elixir: Kaavan mukaisten merkkien poistaminen"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Miksi: 
Koodin kirjoittaminen ja ylläpito voidaan helpottaa poistamalla merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi auttaa säilyttämään koodin selkeyden ja tehokkuuden.

Kuinka tehdä: 
```Elixir
def delete_pattern(pattern, text) do 
  Regex.replace pattern, text, ""
end

IO.puts delete_pattern ~r/[0-9]/, "Olen syntynyt vuonna 1995"
```

Tulostus: "Olen syntynyt vuonna"

Vavvau! Tämä yksinkertainen koodinpätkä poistaa tekstistä kaikki numerot ja palauttaa vain sanallisen osan. Voit käyttää tätä toimintoa esimerkiksi silloin, kun sinun tarvitsee lukea käyttäjältä syötteitä ja haluat poistaa niistä kaikki mahdolliset numerot.

Syvällinen sukellus: 
Tämä yksinkertainen esimerkki käyttää Regex-moduulia, joka on sisäänrakennettu Elixir-ohjelmointikieleen. Se mahdollistaa monimutkaisten kaavojen käytön koodin muokkauksessa ja poistaa tehokkaasti merkkejä. Lisäksi voit käyttää myös Regex.match -toimintoa, joka palauttaa tekstistä vastaavat kohdat kaavan perusteella.

Katso myös: 
Voit oppia lisää Regexin käytöstä Elixirissä ja sen hyödyllisyydestä alla olevista linkeistä:
- [Regex-moduulin dokumentaatio](https://hexdocs.pm/elixir/Regex.html)
- [Vinkkejä Regexin käyttöön Elixirissä](https://dev.to/nagasirena/using-regex-matching-in-elixir-3m1n)
- [Realistisen esimerkin Regexin käytöstä Elixirissä](https://www.poeticoding.com/2-simple-regex-examples-in-elixir-with-string-match/)