---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä & Miksi?"
YAML on datan sarjoituskieli, jota käytetään konfiguraatiotiedostoissa ja datan siirrossa. Ohjelmoijat käyttävät YAMLia, koska se on luettavissa ihmiselle ja helppo muokata, mutta silti koneystävällinen.

## How to:
"Kuinka:"
Elixirissä käytetään YAML:ia `yamerl` -kirjaston avulla. Esimerkiksi voimme purkaa YAML-tiedoston ja muuttaa sen Elixirin kartaksi (map):

```Elixir
# Lisää ensin yamerl riippuvuutena mix.exs-tiedostossa
defp deps do
  [{:yamerl, "~> 0.8.0"}]
end

# Sitten pura YAML-tiedosto
:ok = Application.ensure_all_started(:yamerl)
yaml_content = """
---
foo: bar
number: 1
"""

{:ok, [parsed_yaml]} = :yamerl_constr.string(yaml_content)
parsed_yaml |> Enum.into(%{})
```

Tuloksena on Elixir-kartta:
```
%{"foo" => "bar", "number" => 1}
```

## Deep Dive
"Syvä sukellus":
YAML kehitettiin vuonna 2001 ja on lyhenne sanoista "YAML Ain't Markup Language", mikä korostaa, että se ei ole merkkauskieli. Vaihtoehtoja YAML:lle ovat JSON ja XML. Elixirissä työskentely YAML-tiedostojen kanssa nojaa Erlangin kirjastoihin, kuten `yamerl`, joka on natiivi YAML-parseri Erlangille.

## See Also
"Katso myös":
YAML-spesifikaatio: https://yaml.org/spec/
`yamerl` GitHub-sivu: https://github.com/yakaz/yamerl
Elixir School YAML-oppitunti: https://elixirschool.com/en/lessons/advanced/yaml/