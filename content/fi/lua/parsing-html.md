---
title:                "HTML:n jäsentäminen"
html_title:           "Lua: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML-analysointi on prosessi, jossa ohjelmoijat käyttävät koodia HTML-tiedoston sisällön analysoimiseen ja strukturointiin. Tämän avulla he voivat helposti lukea ja käsitellä HTML-tiedostoja ja saada tarvittavat tiedot niistä. Ohjelmoijat tekevät tätä usein verkkosivujen sisällön hakemista tai data-scrapingia varten.

## Miten:

```Lua
-- Luo muuttuja johon tallennetaan HTML-tiedoston sisältö
local html = [[
<html>
<head>
	<title>Lua HTML-analysointi</title>
</head>
<body>
	<h1>Tervetuloa!</h1>
	<p>Tässä on esimerkki HTML-tiedostosta.</p>
</body>
</html>
]]

-- Kirjastojen lataaminen
local htmlparser = require("htmlparser")

-- Luo uusi HTML-parseri
local parser = htmlparser.parse(html)

-- Tulosta otsikko
print(parser.head.title) -- "Lua HTML-analysointi"

-- Tulosta kappaleen sisältö
print(parser.body.p) -- "Tässä on esimerkki HTML-tiedostosta."
```

## Syvemmälle:

HTML-parsiin liittyy paljon historiaa. Alun perin se oli tarpeen verkkosivujen sisällön esittämiseen selkeämmin ja helpommin ymmärrettävässä muodossa. Nykyään sitä käytetään usein web-skrapingin eli tiedon poimimisen verkkosivuilta tarkoituksena esimerkiksi analysoida tietoa tai luoda uutta sisältöä. HTML-parsereita on myös olemassa useita erilaisia, joista jokaiseen on omat hyötynsä ja haittansa.

## Katso myös:

- [Lua HTML-parseri](https://github.com/msva/lua-htmlparser)
- [HTML-parsiminen Wikissä](https://fi.wikipedia.org/wiki/HTML-parsing)
- [Verkkosivujen data-scraping](https://fi.wikipedia.org/wiki/Web_scraping)