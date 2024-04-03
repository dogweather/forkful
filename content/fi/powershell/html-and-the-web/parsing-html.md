---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:41.403086-07:00
description: "Kuinka: PowerShell ei natiivisti sis\xE4ll\xE4 erityist\xE4 HTML-j\xE4\
  sent\xE4j\xE4\xE4, mutta voit k\xE4ytt\xE4\xE4 `Invoke-WebRequest` cmdletia HTML-sis\xE4\
  ll\xF6n saavuttamiseksi ja\u2026"
lastmod: '2024-03-13T22:44:56.780289-06:00'
model: gpt-4-0125-preview
summary: "PowerShell ei natiivisti sis\xE4ll\xE4 erityist\xE4 HTML-j\xE4sent\xE4j\xE4\
  \xE4, mutta voit k\xE4ytt\xE4\xE4 `Invoke-WebRequest` cmdletia HTML-sis\xE4ll\xF6\
  n saavuttamiseksi ja j\xE4sent\xE4miseksi."
title: "HTML:n j\xE4sennys"
weight: 43
---

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
