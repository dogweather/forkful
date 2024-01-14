---
title:                "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z najważniejszych czynności w programowaniu aplikacji internetowych jest pobieranie stron internetowych. Możesz to robić, aby sparsować dane, zapisać je do bazy danych lub po prostu przeglądać je offline. W tym blog poście omówimy, jak w prosty sposób pobrać stronę internetową za pomocą języka programowania C#.

## Jak to zrobić

Pierwszym krokiem jest zainstalowanie pakietu NuGet called `HtmlAgilityPack`, który pomoże nam w parsowaniu dokumentów HTML. Następnie w naszym kodzie potrzebujemy utworzyć obiekt typu `WebClient` i użyć metody `DownloadString` aby pobrać zawartość strony internetowej. Oto przykładowy kod:

```C#
// Importowanie niezbędnych bibliotek
using System.Net;
using HtmlAgilityPack;

// Tworzenie obiektu WebClient
var client = new WebClient();
// Pobieranie zawartości strony internetowej
var html = client.DownloadString("https://www.example.com/");
// Użycie HtmlAgilityPack do parsowania dokumentu HTML
var document = new HtmlDocument();
document.LoadHtml(html);

// Pobranie elementów ze strony internetowej
var title = document.DocumentNode.SelectSingleNode("//head//title").InnerText;
var paragraphs = document.DocumentNode.SelectNodes("//p");

// Wyświetlenie pobranych elementów
Console.WriteLine("Tytuł strony: " + title);
foreach (var paragraph in paragraphs)
{
    Console.WriteLine("Paragraf: " + paragraph.InnerText);
}
```

Po uruchomieniu tego kodu, powinieneś zobaczyć tytuł i treść paragrafów pobranej strony internetowej.

## Deep Dive

Podczas pobierania strony internetowej musisz pamiętać, że niektóre elementy mogą być wczytywane asynchronicznie, na przykład za pomocą JavaScript. W takim przypadku metoda `DownloadString` nie będzie przechwytywać tych elementów. W takim przypadku możesz użyć Selenium WebDriver, aby przeładować stronę i pobrać jej aktualną zawartość.

## Zobacz także
- Dokumentacja HtmlAgilityPack: https://html-agility-pack.net/
- Dokumentacja WebClient: https://docs.microsoft.com/pl-pl/dotnet/api/system.net.webclient?view=net-5.0