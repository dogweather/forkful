---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

"Pattern matching" on ohjelmoinnissa luotu termi, jolla viitataan prosessiin, jossa merkkijonosta poistetaan määrättyjä kuvioon sopivia merkkejä. Tämä on erittäin hyödyllinen ohjelmoijille, kun he haluavat esimerkiksi puhdistaa dataa tai tehdä säännöllisiä lausekkeita.

## Kuinka tehdä:

Deleting characters in a string that match a pattern in Elixir is quite simple. Here's how you do it:

```Elixir
# Kirjoitetaan funktio
def remove_pattern(string, pattern) do
  String.replace(string, pattern, "")
end

# Funktio käytössä
iex> remove_pattern("Hello, world!", ",")
"Hello world!"
```
Tässä esimerkissä `remove_pattern` funktio ottaa kaksi argumenttia, merkkijonon ja kuviomme. Se palauttaa alkuperäisen merkkijonon, mutta poistaa kaikki kuviona olevat merkit.

## Syvällisempi sukellus:

Pattern matchingilla on pitkä historia ohjelmoinnissa ja se tulee suoraan matematiikasta. Elixir käyttää Erlangin säännöllisiä ilmaisuja, jotka ovat peräisin luvulta 1970.

Joskus kuvion poisto ei välttämättä ole paras ratkaisu. Vaihtoehtoina on esim. kuvion sijainnin tai kuvion sisällä olevien merkkien tarkistaminen. Riippuu siitä, mitä tarvitset tehdä.

Elixirin `String.replace/3` -metodi käyttää Elixirin & Erlangin vakioaloituskirjastoa ja tarttuu säännölliseen lausekkeeseen luodakseen uuden merkkijonon, joka puuttuu poistetusta kuvioista.

## Katso myös:

- Erlangin dokumentaatio säännöllisistä lausekkeista: http://erlang.org/doc/man/re.html
- Elixirin String-moduulin dokumentaatio: https://elixir-lang.org/docs/stable/elixir/String.html
- Elixirin oppaat säännöllisten lausekkeiden käyttöön: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html