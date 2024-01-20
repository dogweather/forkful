---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML:n jäsennys viittaa siihen, kuinka ohjelma interprettaa HTML-koodin rakenteen ja tietojen suhteen. Tämä on tärkeää, jotta voimme hakea, muokata ja manipuloida sivuston tietoja tehokkaasti.

## Miten tehdään:

Katsotaanpa esimerkkiä siitä, kuinka Gleamilla voidaan jäsennellä HTML. Tässä me käytämme `gleam/regex` kirjastoa.

```Gleam
import gleam/regex.{self as regex}


fn main() {
  let html = "<div><h1>Hello, world!</h1></div>"
  let re = regex.from_string("<h1>(.*?)</h1>")
  
  case re {
    Ok(re) ->
      case regex.find(re, html) {
        Some(found) ->
          let data = found.group(1)
          // Tulostaa: "Hello, world!"
          print(data)
          
        None ->
          // Tulostaa: Ei löytynyt otsikkoa
         print("Ei löytynyt otsikkoa")
      }
     Error(_) ->
       print("Virheellinen regex")
  }
}
```

## Syvempi sukellus

HTML-jäsennyksen historia juontaa juurensa WWW:n alkuaikoihin. Sen merkitys on kasvanut eksponentiaalisesti, koska verkkosivustot ovat muuttuneet yksinkertaisista staattisista sivuista monimutkaisiksi dynaamisiksi sovelluksiksi.

Vaihtoehtoja Gleamille HTML:n jäsentämisessä ovat esimerkiksi Pythonin Beautiful Soup tai JavaScriptin Cheerio. Ne saattavat olla sopivampia tiettyjen vaatimusten tai erityisosaamisen mukaan.

Gleamin regex-kirjasto käyttää Elixirin regex kirjastoa taustalla, jolloin saadaan hyödynnettyä Erlangin tehokasta säännöllisten lausekkeiden toteutusta.

## Katso myös

- Gleam Regex kirjaston dokumentaatio[https://hexdocs.pm/gleam_regex/readme.html]
- Erlang regex käyttöohje[http://erlang.org/doc/man/re.html]
- JavaScriptin Cheerio kirjasto GitHub[https://github.com/cheeriojs/cheerio]
- Pythonin Beautiful Soup dokumentaatio[https://www.crummy.com/software/BeautifulSoup/bs4/doc/]