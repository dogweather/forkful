---
title:                "Elixir: Testien kirjoittaminen"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Miksi testaaminen on tärkeää Elixir-ohjelmoinnissa? Onko se vain ylimääräinen askel vai hyödyllinen osa ohjelmistokehitystä? Tässä blogipostauksessa käymme läpi testaamisen tärkeyden ja hyödyt Elixirin ohjelmointikielen näkökulmasta.

## Miten

Testaaminen Elixirissä on helppoa ja välttämätöntä. Testien avulla voidaan varmistaa ohjelman toiminnan oikeellisuus ja ehkäistää mahdollisia bugeja ja virheitä. Alla on muutamia esimerkkejä testien kirjoittamisesta Elixirillä ja niiden tulosteista.

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case

  test "addition" do
    assert Calculator.add(2, 3) == 5
  end

  test "division" do
    assert Calculator.divide(10, 2) == 5
  end
end
```

Testikoodi on hyvä kirjoittaa samassa tiedostossa kuin varsinaisen koodin kanssa, ja ne suoritetaan erillisissä testeissä. Testauksen avulla voidaan myös helposti varmistaa, että uudet muutokset eivät riko olemassa olevaa toiminnallisuutta.

## Syvemmälle

Testaaminen Elixirissä perustuu ExUnit-testikehykseen ja automaattisen testauksen periaatteisiin. Elixirin avoin syntaksirakenne mahdollistaa yksinkertaisten ja luettavien testien kirjoittamisen. ExUnit tarjoaa myös käteviä toimintoja, kuten testien ajamisen tiettyjen funktioiden ja moduulien mukaan.

On myös hyvä huomioida, että testien kirjoittaminen voi tuntua ylimääräiseltä työltä aluksi, mutta se säästää lopulta aikaa ja vaivaa korjaamalla mahdollisia bugeja ja virheitä myöhemmin ohjelman elinkaaren aikana.

## Katso myös

- [Elixir-käsikirja: Testaus](https://elixir-lang.org/getting-started/testing.html)
- [Test-Driven Development Elixirillä](https://medium.com/@jeffkreeftmeijer/test-driven-development-in-elixir-f5143abea963)
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)