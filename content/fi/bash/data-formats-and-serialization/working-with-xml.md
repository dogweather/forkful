---
date: 2024-01-26 04:27:34.691183-07:00
description: "Miten: N\xE4in j\xE4sennet\xE4\xE4n XML:\xE4\xE4 Bashissa. Ty\xF6kalut?\
  \ xmllint ja xmlstarlet. Elementtien l\xE4pik\xE4ynti loopilla? Ehdottomasti. Esimerkki\
  \ ja esimerkkituloste."
lastmod: '2024-03-13T22:44:56.762909-06:00'
model: gpt-4-0125-preview
summary: "N\xE4in j\xE4sennet\xE4\xE4n XML:\xE4\xE4 Bashissa."
title: "XML:n k\xE4sittely"
weight: 40
---

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
