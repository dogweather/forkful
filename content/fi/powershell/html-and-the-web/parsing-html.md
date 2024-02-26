---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:41.403086-07:00
description: "HTML:n j\xE4sent\xE4minen PowerShelliss\xE4 tarkoittaa HTML-sis\xE4\
  ll\xF6n purkamista erityisten tietojen poimimiseksi tai webiin liittyvien teht\xE4\
  vien\u2026"
lastmod: '2024-02-25T18:49:53.691010-07:00'
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sent\xE4minen PowerShelliss\xE4 tarkoittaa HTML-sis\xE4ll\xF6\
  n purkamista erityisten tietojen poimimiseksi tai webiin liittyvien teht\xE4vien\u2026"
title: "HTML:n j\xE4sennys"
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
