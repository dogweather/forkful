---
title:                "Parsing html"
date:                  2024-01-20T15:33:02.616930-07:00
html_title:           "Bash recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML means breaking down HTML content to extract specific data. Programmers do it to automate web scraping, data mining, or to integrate web content into applications.

## How to:
Let's grab some data from a web page. We'll use Invoke-WebRequest and then siphon out what we need.

```PowerShell
# Fetch the page content
$response = Invoke-WebRequest -Uri "http://example.com"

# Parse the HTML content
$parsedHtml = $response.ParsedHtml

# Extract data
# Say we want all the hyperlink texts
$links = $parsedHtml.getElementsByTagName('a') | ForEach-Object { $_.innerText }
$links
```

Sample output:

```
Home
About Us
Services
Contact
```

## Deep Dive
Historically, parsing HTML in PowerShell could be clunky. You had the choice of using regex (notoriously problematic for HTML), COM objects with Internet Explorer, or third-party libraries. Now, PowerShell's Invoke-WebRequest cmdlet simplifies the process, integrating with the Internet Explorer engine to parse HTML — though it's a bit slow and cumbersome.

Alternatives like the HtmlAgilityPack library exist, which is far more robust and fine-tuned for parsing HTML. It requires extra setup but pays off with flexibility and performance.

Implementation-wise, note that PowerShell's approach is not always accurate for dynamic content filled by JavaScript. To handle dynamic content, you might need browser automation tools like Selenium.

## See Also
- [HtmlAgilityPack on GitHub](https://github.com/zzzprojects/html-agility-pack)
- [Selenium with PowerShell](https://github.com/adamdriscoll/selenium-powershell)