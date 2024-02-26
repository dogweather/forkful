---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:34.163592-07:00
description: "Elixiriss\xE4 s\xE4\xE4nn\xF6llisi\xE4 lausekkeita (regex) k\xE4ytet\xE4\
  \xE4n merkkijonojen hakemiseen, t\xE4sm\xE4\xE4miseen ja manipulointiin tiettyjen\
  \ mallien perusteella. Ohjelmoijat\u2026"
lastmod: '2024-02-25T18:49:53.190700-07:00'
model: gpt-4-0125-preview
summary: "Elixiriss\xE4 s\xE4\xE4nn\xF6llisi\xE4 lausekkeita (regex) k\xE4ytet\xE4\
  \xE4n merkkijonojen hakemiseen, t\xE4sm\xE4\xE4miseen ja manipulointiin tiettyjen\
  \ mallien perusteella. Ohjelmoijat\u2026"
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Elixirissä säännöllisiä lausekkeita (regex) käytetään merkkijonojen hakemiseen, täsmäämiseen ja manipulointiin tiettyjen mallien perusteella. Ohjelmoijat hyödyntävät regexiä tehtäviin kuten muotojen validointiin (sähköposti, URL-osoitteet), lokien jäsentämiseen tai datan poimintaan, kiitos sen tehokkuuden ja monipuolisuuden merkkijonojen käsittelyssä.

## Kuinka:

Elixir käyttää `Regex`-moduulia, hyödyntäen Erlangin regex-kirjastoa, regex-operaatioihin. Tässä peruskäyttötapoja:

```elixir
# Mallin täsmääminen - Palauttaa ensimmäisen osuman
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Tuloste: ["hello"]

# Kaikkien osumien löytäminen
all_matches = Regex.scan(~r/\d/, "There are 2 apples and 5 oranges.")
IO.inspect(all_matches) # Tuloste: [["2"], ["5"]]

# Merkkijonon osien korvaaminen
replaced_string = Regex.replace(~r/\s+/, "Elixir on kivaa", "_")
IO.inspect(replaced_string) # Tuloste: "Elixir_on_kivaa"
```

Monimutkaisempia malleja ja toiminnallisuuksia varten saatat harkita kolmannen osapuolen kirjastojen käyttöä, mutta useimpiin ydinteksti- ja mallintäsmäystehtäviin Elixiriin sisäänrakennettu `Regex`-moduuli on varsin tehokas.

Sukupuolineutraaliin täsmäykseen käytä `i`-vaihtoehtoa:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Tuloste: ["Hello"]
```

Regex-lausekkeet voidaan esikääntää tehokkuuden lisäämiseksi, kun niitä käytetään useita kertoja:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Tuloste: ["hello"]
```

Elixir tukee myös nimettyjä kaappauksia, jotka voivat olla erittäin käteviä tiettyjen merkkijonojen osien poimintaan samalla, kun teet koodistasi luettavampaa:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Tuloste: %{"year" => "2023", "month" => "04", "day" => "15"}
```

Tämä lyhyt yleiskatsaus korostaa sitä, kuinka vaivattomasti Elixir käsittelee säännöllisiä lausekkeita, mahdollistaen tehokkaat merkkijonojen manipulointi- ja datanpoimintatekniikat.
