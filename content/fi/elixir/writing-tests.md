---
title:                "Testien kirjoittaminen"
html_title:           "Elixir: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Tervehdys suomalaiset lukijat! Tervetuloa tutustumaan Elixirin testien kirjoittamiseen. Tämä artikkeli käsittelee testien tärkeyttä ja antaa ohjeita niiden kirjoittamiseen.

Testien kirjoittaminen on tärkeää, koska se auttaa meitä varmistamaan koodimme toimivuuden ja estää mahdollisia virheitä tuotantoympäristössä. Hyvin kirjoitetut testit antavat meille luottamusta ja varmuutta siitä, että koodimme toimii halutulla tavalla.

## Näin teet sen

Puhuttaessa testien kirjoittamisesta, on hyvä aloittaa yksinkertaisista esimerkeistä ja lisätä vaikeustasoa asteittain. Alla olevassa esimerkissä olemme luoneet yksinkertaisen funktion, joka laskee kaksi annettua numeroa yhteen.

```elixir
defmodule Calculator do
  def sum(a, b) do
    a + b
  end
end

# Suoritetaan testi
test "sum function returns correct result" do
  assert Calculator.sum(1, 2) == 3
end
```

Koodilohko alkaa ```defmodule Calculator do```, joka määrittelee moduulin nimen "Calculator". Moduulilla on funktio ```sum```, joka laskee kaksi annettua numeroa yhteen. Sitten suoritamme testin, jossa tarkistetaan, että funktion palauttama tulos on halutunlainen.

Seuraavassa esimerkissä käytämme Elixirin sisäänrakennettuja testikirjastoja ja testaamme moduulin ``String`` funktiota ``length``.

```elixir
# Ensin kirjastojen sisällyttäminen testataksesi niitä
ExUnit.start()

# Suoritetaan testi
test "string length function returns correct value" do
  assert String.length("hello") == 5
end
```

Tässä esimerkissä käytimme testikirjaston ```ExUnit.start()``` -komennon aloittaaksemme testien suorittamisen. Sitten suoritamme testin, jossa tarkistamme, että funktion palauttama tulos vastaa odotettua tulosta.

## Syvempi sukellus

Testien kirjoittamisessa on paljon muitakin käytäntöjä ja työkaluja, joita voit hyödyntää. On tärkeää tutkia erilaisia lähestymistapoja ja löytää ne keinot, jotka toimivat parhaiten omalle tiimille ja projektille. 

Yksi tärkeä asia, johon kannattaa kiinnittää huomiota, on testeihin käytetty aika. On hyvä löytää tasapaino koodin laadun ja testien kirjoittamiseen käytetyn ajan välillä. Liian vähemmän testeillä voi olla kielteisiä vaikutuksia koodin toimivuuteen, mutta liikaa testeihin käytetty aika voi hidastaa kehitysprosessia.

Toinen tärkeä asia on testien säilyttäminen ajan tasalla muutosten yhteydessä. Testeistä ei ole hyötyä, jos ne eivät heijasta koodin nykyistä tilaa. On hyvä käydä läpi testejä säännöllisesti ja päivittää ne tarvittaessa.

## Katso myös

- [ExUnit - Elixirin testikirjasto](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Kirjoittamalla testejä - Elixirin virallinen opas](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-coverage.html)
- [18 asiaa jotka auttavat kehittämään Elixir-taitojasi](https://www.cowbearcode.com/18