---
title:                "Lokitus"
date:                  2024-01-26T01:03:59.643646-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/logging.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lokit ovat käytännössä tapamme kirjata, mitä ohjelmissamme tapahtuu. Se on kuin meillä olisi pieni musta laatikko; kun asiat menevät pieleen (ja usko minua, niin tulee käymään), lokit ovat arvokkaita selvittäessämme, mitä tapahtui, diagnosoimme ongelmia ja optimoimme suorituskykyä.

## Miten:
Gleamissa tyypillisesti ottaisit käyttöön lokikirjaston – kielen valmiissa toiminnoissa ei ole erillistä loki-mekanismia. Kuvitellaan, että käytämme hypoteettista `gleam_logger`-pakettia. Näin voisit integroida sen:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("Sovellus käynnistyy!")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Laskenta onnistui", value)
    Error(err) -> 
      gleam_logger.error("Laskenta epäonnistui", err)
  }
}
```

Odotetut lokitulosteet näyttäisivät tältä:

```
INFO: Sovellus käynnistyy!
DEBUG: Laskenta onnistui 42
ERROR: Laskenta epäonnistui Syy: Jakaminen nollalla
```

## Syväsukellus
Lokinpidon taito on ollut olemassa ohjelmoinnin alkuaikojen jälkeen. Järjestelmänvalvojat saivat kirjaimellisesti lokit tietokoneelta - varmistaakseen, että kaikki sujui sujuvasti. Pikakelauksella nykyaikaan, ja lokitus on digitalisoitu ja muodostunut ohjelmistokehityksen ydinosa-alueeksi.

Vaikka Gleam, joka on suhteellisen nuori kieli ja tähtää Erlang-ekosysteemiin, ei omaa valmista lokituskehytösuojaansa, voit hyödyntää kypsää Erlangin lokitusvälineistöä tai muita yhteisön tarjoamia kirjastoja. Jokaisessa on erilaisia ominaisuuksia ja kompromisseja: jotkut voivat tarjota rakenteellista lokitusta, toiset sopivat yksinkertaiseen tekstulosteeseen.

Nyt, kysymys lokitusvälineistön toteuttamisesta: Onko se yksinkertaista? Ensivilkaisulla, kyllä. Mutta kun raaputtaa pintaa syvemmälle, tulee vastaan samanaikaisuuden hallintaa, I/O pullonkauloja, lokikiertoa, muotoilustandardointia (kuten JSON rakenteellista lokitusta varten), tasosuodatusta ja mahdollisesti hajautettua jäljitystä. Lisäksi funktionaalisessa paradigmassa haluat yleensä, että sivuvaikutukset (kuten lokitus) käsitellään ennustettavalla ja hallitulla tavalla.

## Katso Myös
Tässä on paikkoja, joissa voit oppia lisää lokituksen ydinkohdista Gleamissa ja sen ympäröivässä ekosysteemissä:
- [Erlangin :logger dokumentaatio](http://erlang.org/doc/apps/kernel/logger_chapter.html): Koska Gleam kääntyy Erlangiksi, tämä on suoraan sovellettavissa.
- [Gleamin vakio kirjaston dokkarit](https://hexdocs.pm/gleam_stdlib/): Päivityksistä kaikkiin lokitusvälineisiin, joita saatetaan lisätä.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): Kuraattoreiden lista resursseista, joka saattaa sisältää lokikirjastoja, kun niitä tulee saataville.