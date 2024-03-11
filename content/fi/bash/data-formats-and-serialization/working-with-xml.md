---
date: 2024-01-26 04:27:34.691183-07:00
description: "XML:n k\xE4sittelyyn kuuluu j\xE4sennys, tietojen poiminta ja manipulointi\
  \ Laajennettavassa Merkint\xE4kieless\xE4 (Extensible Markup Language). Ohjelmoijat\u2026"
lastmod: '2024-03-11T00:14:30.719995-06:00'
model: gpt-4-0125-preview
summary: "XML:n k\xE4sittelyyn kuuluu j\xE4sennys, tietojen poiminta ja manipulointi\
  \ Laajennettavassa Merkint\xE4kieless\xE4 (Extensible Markup Language). Ohjelmoijat\u2026"
title: "XML:n k\xE4sittely"
---

{{< edit_this_page >}}

## Mikä & Miksi?
XML:n käsittelyyn kuuluu jäsennys, tietojen poiminta ja manipulointi Laajennettavassa Merkintäkielessä (Extensible Markup Language). Ohjelmoijat kamppailevat XML:n kanssa, koska se on laajalti käytetty tietojenvaihtoformaatti konfiguraatioille, API:lle ja muulle.

## Miten:
Näin jäsennetään XML:ää Bashissa. Työkalut? xmllint ja xmlstarlet. Elementtien läpikäynti loopilla? Ehdottomasti. Esimerkki ja esimerkkituloste:

```bash
# Olettaen, että xmlstarlet on asennettu
# Asenna komennolla: apt-get install xmlstarlet

# Jäsennetään XML-sisältöä
cat <<EOF > sample.xml
<hedelmat>
  <hedelma nimi="Omena"/>
  <hedelma nimi="Banaani"/>
</hedelmat>
EOF

# Poimitaan nimet xmlstarletilla
xmlstarlet sel -t -m "//hedelma" -v "@nimi" -n sample.xml

# Tulosteen pitäisi olla:
# Omena
# Banaani
```

## Syväsukellus
90-luvulla XML nousi esiin yksinkertaisempana vaihtoehtona SGML:lle, mutta rakenteellisempana kuin HTML. Nyt sillä on seuraa – esimerkiksi JSON, YAML. Mutta XML on yhä mukana pelissä, erityisesti konfiguraatioissa ja SOAP-pohjaisissa verkkopalveluissa.

Työkalujen osalta xmllint on mukava XML-validoinnissa, xpath-kyselyissä. xmlstarlet on Sveitsin armeijan linkkuveitsi XML-tempuille – kysely, muokkaus, validointi, muunnos. Bash-skripteissä ne ovat supersankareita XML-tehtävissä.

Teknisen puolen syvyyksissä xmllint käyttää libxml2:ta – C-pohjaista XML-jäsennintä. Se on nopea, mutta virheviestit? Kryptisiä. Ja xmlstarlet? Rekursiiviset mallit ja EXSLT-tuki. Mielenkiertoista, mutta tehokasta.

## Katso myös
- [xmlsoft.org](http://xmlsoft.org/): Libxml2 ja xmllint -juttuja.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Todellisen maailman ongelmat ja ratkaisut.
- [W3Schools XML-opas](https://www.w3schools.com/xml/): XML:n perusteet.
