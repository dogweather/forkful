---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:33:12.341706-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
HTML:n jäsentäminen on prosessi, jossa HTML-sisällöstä poimitaan tietoja. Koodaajat tekevät tämän datan hyödyntämiseen, esimerkiksi verkkosivujen sisällön kaapaukseen tai automatisoituun testaukseen.

## How to: (Miten tehdä:)
```PowerShell
# Asenna ensin tarvittava HtmlAgilityPack-moduuli
Install-Package HtmlAgilityPack

# Lataa HTML-sivun sisältö
$html = Invoke-WebRequest -Uri 'http://example.com'

# Lataa HtmlAgilityPack
Add-Type -Path 'path\to\HtmlAgilityPack.dll'

# Luo HTML-dokumentin ja jäsentää sen
$doc = New-Object HtmlAgilityPack.HtmlDocument
$doc.LoadHtml($html.Content)

# Hae tiettyjä elementtejä XPathilla
$nodes = $doc.DocumentNode.SelectNodes('//h1')
foreach($node in $nodes){
    Write-Output $node.InnerText
}
```
Esimerkkitulostus:
```
Tervetuloa esimerkkisivulle
```

## Deep Dive (Syväsukellus):
HTML:n jäsentäminen on ollut tarpeen siitä asti, kun verkkosivuja alettiin laajemmin hyödyntää dataa varten. Vaihtoehtoiset työkalut kuten JavaScriptin `DOMParser` tai Pythonin `BeautifulSoup` tarjoavat samankaltaista toiminnallisuutta. PowerShellin etu on sen vahva integroituvuus Windows-ympäristössä. HtmlAgilityPack on suosittu .NET-kirjasto HTML-dokumenttien käsittelyyn, ja sen avulla voidaan käyttää XPath-kyselyä sopivien elementtien löytämiseen ja niiden sisällön käsittelyyn. Tämä tekee jäsentämisprosessista joustavan ja voimakkaan.

## See Also (Katso Myös):
- [HtmlAgilityPack-kotisivu](https://html-agility-pack.net/)
- [PowerShell Gallery](https://www.powershellgallery.com/)
- [XPath-tutorial](https://www.w3schools.com/xml/xpath_intro.asp)
