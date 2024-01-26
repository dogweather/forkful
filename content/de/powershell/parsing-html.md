---
title:                "HTML parsen"
date:                  2024-01-20T15:33:14.141709-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML bedeutet, den HTML-Code zu analysieren, um spezifische Daten herauszuziehen. Programmierer machen das, um Informationen aus Webseiten zu extrahieren, sei es für Datenanalyse, Web Scraping oder um automatisierte Tests für Webanwendungen zu erstellen.

## Wie geht das?
In PowerShell kannst du das `Invoke-WebRequest` Cmdlet nutzen, um HTML-Inhalte einer Webseite abzurufen. Mit `Invoke-WebRequest` erhältst du ein Objekt, das Methoden und Eigenschaften bietet, um auf die verschiedenen Teile des HTML-Dokuments zuzugreifen. Hier ein einfaches Beispiel, wie man den Titel einer Webseite bekommt:

```PowerShell
$response = Invoke-WebRequest -Uri "https://example.com"
$title = $response.ParsedHtml.title
$title.innerText
```

Die Ausgabe wäre der Text des `<title>`-Tags von `https://example.com`.

Um komplexere HTML-Elemente zu durchsuchen, kannst du CSS-Selektoren verwenden:

```PowerShell
$links = $response.ParsedHtml.querySelectorAll('a')
foreach ($link in $links) {
    $link.href
}
```

Dies zeigt die `href`-Attribute aller `<a>`-Tags auf der Seite.

## Tiefgang
Früher war HTML-Parsing in PowerShell umständlicher; man musste externe Bibliotheken wie HTML Agility Pack über .NET-Interop laden. PowerShell hat sich allerdings weiterentwickelt, und `Invoke-WebRequest` sowie `Invoke-RestMethod` machen es einfacher, da sie direkt Objekte zurückgeben, die man durchsuchen kann.

Es gibt Alternativen wie die Verwendung der HtmlAgilityPack-Bibliothek in .NET, die speziell für das Parsen von HTML ausgelegt ist und mehr Flexibilität bietet, oder Puppeteer und Selenium, die für Browserautomatisierung konzipiert sind.

Das Parsen mittels PowerShell ist nicht immer fehlerfrei, da Webseiten komplexe JavaScript-Logik enthalten können, die das DOM nach dem Laden verändert. Für solche Fälle werden oft Headless-Browser wie Chrome oder Firefox in Kombination mit Automatisierungstools benutzt.

## Siehe auch
- [Invoke-WebRequest Hilfe und Beispiele](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [HtmlAgilityPack GitHub Repository](https://github.com/zzzprojects/html-agility-pack)
- [Puppeteer GitHub Repository](https://github.com/puppeteer/puppeteer)
- [Selenium WebDriver](https://www.selenium.dev/documentation/webdriver/)
