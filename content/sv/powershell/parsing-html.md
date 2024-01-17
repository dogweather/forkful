---
title:                "Analysera html"
html_title:           "PowerShell: Analysera html"
simple_title:         "Analysera html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/parsing-html.md"
---

{{< edit_this_page >}}

### Vad & Varför?
Att 'parsa' HTML betyder att extrahera specifika delar av en HTML-kodad webbsida för att kunna använda dem i ditt PowerShell-program. Detta gör det möjligt att automatisera uppgifter som att extrahera data från en webbplats eller skapa rapporter baserat på webbsideinnehåll. Det sparar också tid och minimerar risken för misstag som kan uppstå vid manuell insamling av data.

### Hur man gör:
```PowerShell
# Ladda ner webbsidan som en sträng
$str = Invoke-WebRequest -Uri "https://www.examplewebsite.com/"
# Parsa den specifika delen av HTML som du är intresserad av
$specificPart = $str.ParsedHtml.getElementById("specific-id").innerHTML
# Skriv ut den parsade delen
Write-Output $specificPart
```

**Exempel på utmatning:** Om den specifika delen du är intresserad av är en lista med produkter som är formaterad som HTML-tabell, kan du använda följande kod för att extrahera data om produkterna och skriva ut dem i konsolen.

```PowerShell
# Ladda ner webbsidan som en sträng
$str = Invoke-WebRequest -Uri "https://www.examplewebsite.com/"
# Välj den första tabellraderad som innehåller produktnamnen
$products = $str.ParsedHtml.getElementsByTagName("tr") | Select-Object -First 1
# Extrahera produktnamnen och skriv ut dem
foreach ($product in $products){
    $productName = $product.getElementsByTagName("td")[0].innerText
    Write-Output $productName
}
```

**Exempel på utmatning:**
* Produkt 1
* Produkt 2
* Produkt 3
* ...

### Djupdykning:
Parsning av HTML är en användbar teknik som har funnits länge och finns också i andra programmeringsspråk som Python och PHP. I PowerShell, kan du använda `Invoke-WebRequest` cmdleten för att hämta och ladda ner en webbsida som en sträng, och sedan använda det inbyggda objektet `$str.ParsedHtml` för att få åtkomst till olika element på webbsidan.

Det finns också alternativa metoder för att parsning av HTML i PowerShell, såsom att använda tredjepartsmoduler som "HtmlAgilityPack". Dessa alternativ kan vara mer robusta och hanterar mer komplext HTML, men de kräver att du installerar och importerar modulen för att använda den.

Implementationen av parsning av HTML i PowerShell är relativt enkel, men det kräver också förståelse för HTML-struktur och sökning av specifika element baserat på deras ID eller klassnamn.

### Se också:
- [Microsoft Dokumentation om 'Invoke-WebRequest'](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [HtmlAgilityPack Modul](https://www.powershellgallery.com/packages/HtmlAgilityPack/)