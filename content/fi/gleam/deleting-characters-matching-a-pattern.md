---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Pattern matching - tai kuvion sovittaminen – on ohjelmoinnissa käytetty tekniikka havaita ja poistaa merkkejä, jotka vastaavat jonkinlaista sääntöä tai kuviota. Ohjelmoijat tekevät tämän siistikseen dataa, esimerkiksi poistaakseen ei-toivotut tai turhat merkit.

## Miten näin:

```Gleam
import gleam/regex

fn delete_chars_matching_pattern(s: String, pattern: String) {
  let re = regex.from_string(pattern)
  case re {
    Ok(re) -> 
      let s = regex.replace(re, s, "", global: True)
      case s {
        Ok(s_filled) -> s_filled
        Error(e) -> e
      }
    Error(e) -> e
  } 
}

fn main() {
  let text = "Hei maailma 123 !!!"
  let pattern = "[^A-Za-z ]"
  
  let result = delete_chars_matching_pattern(text, pattern)
  io.println(result)
}
```
Esimerkin tuloste:

```Gleam
"Hei maailma     "
```
## Deep Dive

Pattern matching -tekniikan kehitys juontaa juurensa 1950-luvun algoritmeihin ja se on ollut tärkeä osa useimpia ohjelmointikieliä. Gleamissa käytetään Erlangin rakentamaa regex-kirjastoa.

Vaihtoehtoiseen tapaan kuuluvat string-menetelmät, kuten `replace` tai `remove`. Kuitenkin, pattern matching on joustavampi ja tehokkaampi erilaisten ja monimutkaisten kuvioasettelujen käytössä. 

Teknisesti, pattern matching toimii vertaamalla merkkijonoja säännölliseen ilmaisuun. Käyttämällä regex-kirjastoa ja regex.replace-metodia, voit poistaa kuviota vastaavat merkit.

## Katso myös:

* Regular Expressions guide: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
* Erlang Regex library: [https://erlang.org/doc/man/re.html](https://erlang.org/doc/man/re.html)