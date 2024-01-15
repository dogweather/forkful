---
title:                "Html-tietojen jäsentäminen"
html_title:           "Gleam: Html-tietojen jäsentäminen"
simple_title:         "Html-tietojen jäsentäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi
Jos haluat pystyä keräämään tietoa verkkosivuilta tai ohjelmallisesti käsitellä HTML-tiedostoja, niin Gleam-kielellä voit helposti suorittaa HTML-parsimistotehtäviä.

## Kuinka
Gleam-kielellä voit käyttää [htmlparse](https://hexdocs.pm/gleam/Htmlparse.html) -funktiota HTML-parsimiseen. Se palauttaa tiedot, joihin voit suoraan soveltaa Gleam-koodiasi.

```Gleam
let html = "<div><p>Hello, world!</p></div>"
let parsed_html = htmlparse.parse(html)

// Tuloste:
// parsed_html = Ok([Div([],[P([], [Text("Hello, world!")])])])
```

Voit käyttää `match` -lauseketta käsittelemään palautetun tiedon eri elementtejä. Esimerkiksi, jos haluat tulostaa ` <p>` -tagin sisällön:

```Gleam
match parsed_html {
  Ok([Div(_, [P(_, text) | _]) | _]) => println(text) // Tulostaa "Hello, world!"
  _ => println("Ei löydetty <p> -tagia")
}
```

## Syväsyventyminen
HTML-parsiminen tapahtuu [htmlparse](https://hexdocs.pm/gleam/Htmlparse.html) moduulin avulla, joka käyttää [html5ever](https://github.com/servo/html5ever) kirjastoa. Tämän avulla Gleam-kielellä voidaan luoda vankka, nopea ja luotettava HTML-parsija. Tarkempia tietoja löydät [htmlparse](https://hexdocs.pm/gleam/Htmlparse.html) ja [html5ever](https://github.com/servo/html5ever) dokumentaatiosta.

## Katso myös
- [html5ever dokumentaatio](https://github.com/servo/html5ever)
- [htmlparse moduulin dokumentaatio](https://hexdocs.pm/gleam/Htmlparse.html)