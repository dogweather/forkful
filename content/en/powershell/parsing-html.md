---
title:                "Parsing html"
html_title:           "PowerShell recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/parsing-html.md"
---

{{< edit_this_page >}}

##What & Why?
Parsing HTML is the skill of extracting information from HTML code, which is the language used to create web pages. Programmers often need to parse HTML in order to gather data from websites or automate tasks. This can save a lot of time and effort by eliminating the need to manually copy and paste data from a website.

##How to:
To parse HTML in PowerShell, we can use the `Invoke-WebRequest` cmdlet. This cmdlet creates a web request to a specified URL and returns the HTML code. We can then use the `Select-String` cmdlet to extract the desired information using regular expressions.

Example code:

```
$url = "https://www.example.com"
$request = Invoke-WebRequest $url
$request.Content | Select-String -Pattern '<div class="title">(.+?)</div>' -AllMatches |
ForEach-Object {$_.Matches} | ForEach-Object {$_.Groups[1].Value}
```

This code will retrieve the titles of all the articles on the website "www.example.com" by selecting the strings that match the specified pattern. It uses regular expressions to locate the desired information within the HTML code.

Sample output:

```
Article 1 Title
Article 2 Title
Article 3 Title
```

##Deep Dive:
Parsing HTML has been a common programming skill since the early days of the internet, when websites were simple and primarily used for displaying text and images. As websites have become more complex, the need for parsing HTML has only increased.

There are several alternatives to parsing HTML in PowerShell, such as using a third-party HTML parsing library or using other programming languages like Python. However, PowerShell's native web cmdlets make it a convenient and efficient option for parsing HTML.

When parsing HTML, it is important to have a basic understanding of regular expressions. These are patterns used to identify specific strings within a larger string of text, and they are commonly used in web scraping and data extraction.

##See Also:
- Microsoft Docs: [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- Microsoft Docs: [Select-String](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/select-string)
- Regular-Expressions.info: [Regular Expressions Tutorial](https://www.regular-expressions.info/tutorial.html)