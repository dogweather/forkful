---
date: 2024-01-26 04:34:24.945957-07:00
description: "Miten: XML, eli Laajennettava Merkint\xE4kieli, on ollut olemassa 90-luvun\
  \ lopusta l\xE4htien ja on edelleen laajalti k\xE4ytetty muoto j\xE4sennetyille\
  \ tiedoille.\u2026"
lastmod: '2024-04-05T22:51:10.957404-06:00'
model: gpt-4-0125-preview
summary: "XML, eli Laajennettava Merkint\xE4kieli, on ollut olemassa 90-luvun lopusta\
  \ l\xE4htien ja on edelleen laajalti k\xE4ytetty muoto j\xE4sennetyille tiedoille."
title: "XML:n k\xE4sittely"
weight: 40
---

## Miten:
```PowerShell
# XML-tiedoston lataaminen muuttujaan
[xml]$xmlContent = Get-Content 'polku\tiedostoonne\file.xml'

# XML-solmujen käyttäminen
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Otsikko: $($book.title)"
}

# Uuden XML-elementin luominen
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# XML-tiedoston tallentaminen takaisin tiedostoon
$xmlContent.Save('polku\tiedostoonne\päivitetty\file.xml')
```
Esimerkkituloste:
```
Otsikko: PowerShell-ohjelmointi
Otsikko: XML:n perusteet
```

## Syväsukellus
XML, eli Laajennettava Merkintäkieli, on ollut olemassa 90-luvun lopusta lähtien ja on edelleen laajalti käytetty muoto jäsennetyille tiedoille. PowerShell yksinkertaistaa XML:n käsittelyä verrattuna perinteisiin jäsentämismenetelmiin; se muuntaa XML:n suoraan objekteiksi, antaen sinun olla vuorovaikutuksessa elementtien kanssa tutun pistenotaation kautta.

Vaihtoehtoja XML:lle sisältävät JSONin, YAML:n tai mukautetut datamuodot. Esimerkiksi JSON on saavuttanut suosiota kevyen luonteensa ja helppokäyttöisyytensä vuoksi web-teknologioiden kanssa. Kuitenkin XML:n laajennetut ominaisuudet kuten nimiavaruudet, kaaviot ja XSLT-käsittely tekevät siitä usein paremman vaihtoehdon monimutkaisille asiakirjoille tai toimialastandardeille.

PowerShell käyttää .NET Frameworkin XML-kyvykkyyksiä XML-käsittelyynsä. Tämä tarkoittaa, että kyse ei ole vain yksinkertaisista luku-kirjoitusoperaatioista; voit myös työskennellä XML-kaavioilla validointiin, käyttää XPathia kyselyihin ja suorittaa XSLT-muunnoksia, kaikki PowerShellin kautta.

## Katso myös
- [W3Schoolsin XML-opas](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
