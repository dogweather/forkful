---
title:                "HTML:n jäsennys"
aliases:
- /fi/powershell/parsing-html/
date:                  2024-02-03T19:12:41.403086-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
HTML:n jäsentäminen PowerShellissä tarkoittaa HTML-sisällön purkamista erityisten tietojen poimimiseksi tai webiin liittyvien tehtävien automatisoimiseksi. Ohjelmoijat tekevät näin vuorovaikuttaakseen verkkosivujen kanssa, kaapiakseen web-sisältöä tai automatisoidakseen lomakkeiden lähetyksiä ja muita web-vuorovaikutuksia tarvitsematta web-selainta.

## Kuinka:

PowerShell ei natiivisti sisällä erityistä HTML-jäsentäjää, mutta voit käyttää `Invoke-WebRequest` cmdletia HTML-sisällön saavuttamiseksi ja jäsentämiseksi. Monimutkaisempaan jäsentämiseen ja manipulointiin voidaan käyttää HtmlAgilityPackia, suosittua .NET-kirjastoa.

### Käyttäen `Invoke-WebRequest`-komentoa:

```powershell
# Yksinkertainen esimerkki otsikoiden noutamiseksi verkkosivulta
$response = Invoke-WebRequest -Uri 'http://example.com'
# Käytä ParsedHtml-ominaisuutta DOM-elementtien käyttämiseksi
$title = $response.ParsedHtml.title
Write-Output $title
```

Esimerkkituloste:

```
Esimerkkialue
```

### Käyttäen HtmlAgilityPackia:

Ensiksi, sinun täytyy asentaa HtmlAgilityPack. Voit tehdä tämän via NuGet Package Manager:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

Sen jälkeen, voit käyttää sitä PowerShellissä HTML:n jäsentämiseen:

```powershell
# Lataa HtmlAgilityPack-kirjasto
Add-Type -Path "polku\kohteeseen\HtmlAgilityPack.dll"

# Luo HtmlDocument-objekti
$doc = New-Object HtmlAgilityPack.HtmlDocument

# Lataa HTML tiedostosta tai web-pyynnöstä
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# Käytä XPathia tai muita kyselymetodeja elementtien poimimiseksi
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

Esimerkkituloste:

```
Tervetuloa sivustolle Example.com!
```

Näissä esimerkeissä `Invoke-WebRequest` on paras yksinkertaisiin tehtäviin, kun taas HtmlAgilityPack tarjoaa paljon rikkaamman joukon ominaisuuksia monimutkaiseen HTML:n jäsentämiseen ja manipulointiin.
