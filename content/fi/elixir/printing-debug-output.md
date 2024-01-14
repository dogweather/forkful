---
title:                "Elixir: Debug-tulosteen tulostaminen"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Tiedämme kaikki sen tunteen, kun koodimme ei toimi ja emme ymmärrä miksi. Tässä tapauksessa debug-tulostus voi olla hyödyllinen työkalu. Se auttaa meitä ymmärtämään tarkemmin ohjelmamme toimintaa ja löytämään mahdolliset virheet.

## Miten

Ensimmäiseksi tarvitsemme `IO` moduulin. Tämä moduuli sisältää kaiken tarvittavan debug-tulostuksen tekemiseen. Käytämme `IO.inspect` funktiota, joka ottaa parametriksi haluamamme arvon ja tulostaa sen konsoliin.

```Elixir
IO.inspect("Hei maailma")
```

Tämä yksinkertainen esimerkki tulostaa tekstin "Hei maailma" konsoliin. Voimme myös antaa useampia arvoja, joita haluamme tarkastella, kuten lista tai kartta.

```Elixir
lista = [1, 2, 3]
kartta = %{nimi: "John", ikä: 30}
IO.inspect(lista, kartta)
```

Tämä tulostaa sekä listan että kartan sisällön konsoliin.

## Syväsukellus

`IO.inspect` funktiolla on myös muita hyödyllisiä vaihtoehtoja. Voimme esimerkiksi määrittää, haluammeko nähdä tulostuksen vain kerran, suorituskokonaisuuden aikana vai joka kierroksella.

Voimme myös käyttää `:label` argumenttia, jonka avulla voimme antaa nimen tulostukselle ja siten helpottaa sen tunnistamista ohjelmamme suorituksen aikana. Esimerkiksi:

```Elixir
IO.inspect(nimi, label: "Nimi")
```

Tulostuksesta tulee näyttämään tältä:

```bash
Nimi: "John"
```

On myös mahdollista käyttää `:pretty` argumenttia, joka muotoilee tulostuksen helpommin luettavaksi. Ja jos haluat poistaa debug-tulostukset kokonaan, voit yksinkertaisesti korvata `IO.inspect` funktion `:nopretty` argumentilla.

## Katso myös

- [Elixir IO moduuli](https://hexdocs.pm/elixir/IO.html)
- [Elixir Debug-tulostus](https://elixir-lang.org/getting-started/debugging.html)
- [Elixir Code BEAM](https://www.erlang-solutions.com/blog/erlang-and-elixir-debugging-and-the-code-beam.html)