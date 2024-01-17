---
title:                "Downloading a web page"
html_title:           "PowerShell recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page refers to the process of retrieving the content and code of a webpage from the internet. This is an important task for programmers as it allows them to access and manipulate the data on a webpage, which can be used for various purposes such as scraping data, automating tasks, or performing web-related tasks in their scripts or applications.

## How to:

To download a web page using PowerShell, we can use the `Invoke-WebRequest` cmdlet. Here is an example of how to download the content of the Google homepage and store it as a variable:

```
$content = Invoke-WebRequest -Uri "https://www.google.com/"
```

To view the HTML code of the downloaded page, we can use the `Content` property of the `$content` variable:

```
$content.Content
```

To save the downloaded page as a file, we can use the `Out-File` cmdlet:

```
$content | Out-File -FilePath "google.html"
```

To download specific elements from a webpage, we can use the `Find` method and specify the HTML tags or classes of the elements we want to retrieve:

```
$links = $content.ParsedHtml.getElementsByTagName("a")
```

## Deep Dive:

Downloading web pages using PowerShell has become easier with the introduction of the `Invoke-WebRequest` cmdlet in version 3.0. In older versions, developers had to use .NET libraries or scripting tools to achieve the same result.

There are also other alternatives to downloading web pages using PowerShell, such as using the `WebRequest` .NET class or using other scripting languages like Python or JavaScript.

When downloading web pages, it is important to consider things like security, error handling, and performance. For example, using the `UserAgent` parameter in `Invoke-WebRequest` can help in preventing firewalls from blocking the request.

## See Also:

- [PowerShell documentation for Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [PowerShell documentation for Out-File](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file)
- [PowerShell documentation for ParsedHtml](https://docs.microsoft.com/en-us/windows/win32/inetsdk/parsedhtml-object)