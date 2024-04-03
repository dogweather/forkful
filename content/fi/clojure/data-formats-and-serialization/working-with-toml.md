---
date: 2024-01-26 04:20:32.032181-07:00
description: "Kuinka: Tehd\xE4ksesi t\xF6it\xE4 TOML:n kanssa Clojuressa tarvitset\
  \ kirjaston, kuten `clj-toml`. Lis\xE4\xE4 se ensin `deps.edn`-tiedostoosi."
lastmod: '2024-03-13T22:44:56.207586-06:00'
model: gpt-4-0125-preview
summary: "Tehd\xE4ksesi t\xF6it\xE4 TOML:n kanssa Clojuressa tarvitset kirjaston,\
  \ kuten `clj-toml`."
title: "Ty\xF6skentely TOML:n kanssa"
weight: 39
---

## Kuinka:
Tehdäksesi töitä TOML:n kanssa Clojuressa tarvitset kirjaston, kuten `clj-toml`. Lisää se ensin `deps.edn`-tiedostoosi:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Sitten jäsennä jotain TOML:aa:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; Hae otsikko jäsennetystä TOML:sta
(println (:title parsed-config)) ;; Tuloste: TOML Example
```

TOML:n luomiseksi:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; Tuloste: title = "TOML Example"
```

## Syväsukellus
TOML luotiin vuonna 2013 Tom Preston-Wernerin, GitHubin perustajajäsenen, toimesta yksinkertaisemmaksi vaihtoehdoksi YAML:lle ja JSON:lle konfiguraatiotiedostoissa. Sen tavoitteena on selkeys ja se pyrkii olemaan speksi, jonka ihmiset voivat lukea ilman lisätyökaluja.

Vaikka JSON:ia käytetään usein API:issa ja web-sovelluksissa, ja YAML voi muuttua monimutkaiseksi viitteiden ja skriptausmahdollisuuksien kanssa, TOML erottuu keskittymisellään yksinkertaisiin, taulukkoperusteisiin rakenteisiin. Tämä yksinkertaisuus tekee siitä erityisen suositun Rust-yhteisössä ja muissa moderneissa kieliympäristöissä.

Clojure, keskittyen yksinkertaisuuteen ja käytännöllisyyteen, parittuu hyvin TOML:n kanssa konfiguraatiossa. `clj-toml` tai vaihtoehtoiset kirjastot kurovat umpeen kuilun. Ne kääntävät TOML:n staattiset data Clojuren dynaamiseen, funktionaaliseen maailmaan.

## Katso Myös
- TOML:n GitHub-säilö: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` Clojarsissa: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure Docs: [clojure.org](https://clojure.org/guides/getting_started)
- Johdatus `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
