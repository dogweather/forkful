---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:31:27.791692-07:00
simple_title:         "HTML:n jäsentäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? 
Mikä & Miksi?

Parsing HTML tarkoittaa HTML-koodin lukemista ja siitä tiedon irti saamista. Ohjelmoijat parsivat HTML:ää, kun haluavat hyödyntää tai muokata web-sivujen dataa.

## How to:
Kuinka toimia:

Fish shell ei ole ihanteellinen työkalu HTML:n parsimiseen, mutta pystyt tekemään yksinkertaista data-ekstraktiota käyttäen komentorivin työkaluja. Oletetaan, että haluat löytää kaikki linkit tietyistä HTML-tiedostoista. Tässä helppo esimerkki käyttäen `grep`- ja `sed`-työkaluja:

```Fish Shell
cat your_file.html | grep -oP '(?<=href=")[^"]*' | sed 's/&amp;/\&/g'
```

Tämä tulostaa näytölle kaikki `href`-attribuutit, korvaten HTML-koodatun `&`-merkin tavallisella `&`-merkillä.

## Deep Dive
Syvä sukellus:

Historiallisesti, komentorivin työkalut eivät ole olleet suunniteltuja HTML:n käsittelyyn, koska HTML:n rakenteet voivat olla monimutkaisia ja muuttuvia. Ohjelmissa kuten Python tai JavaScript, on erityisiä kirjastoja, kuten Beautiful Soup tai jsdom, jotka on tehty juuri HTML:n käsittelyyn ja tarjoavat paljon vahvemmat ja joustavammat tavat käsitellä webin dataa.

Käytettäessä Fish Shelliä, saatat turvautua ulkopuolisiin työkaluihin kuten `pup`, joka on komentorivipohjainen HTML-parseri. Siinä missä `grep` ja `sed` ovat tehokkaita yksinkertaisiin tekstioperaatioihin, `pup` tarjoaa spesifejä komentoja DOM-mallin mukaiseen käsittelyyn.

## See Also
Katso myös:

- [Beautiful Soup documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [`pup` GitHub repository](https://github.com/ericchiang/pup)
- [W3Schools HTML Parser Tutorial](https://www.w3schools.com/xml/xml_parser.asp)
