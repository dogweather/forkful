---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych jest powszechną czynnością w świecie programowania. Często potrzebujemy danych zawartych na stronie lub analizujemy jej zawartość. Dlatego nauka, jak pobrać stronę internetową za pomocą C#, może być bardzo przydatna dla programistów.

## Jak to zrobić

Pierwszym krokiem jest utworzenie projektu w C# i dodanie odpowiednich referencji do kodu. Następnie wykorzystujemy klasę WebClient, aby wykonać żądanie HTTP i pobrać zawartość strony.

```C#
using System.Net;

//tworzymy obiekt WebClient
WebClient client = new WebClient();

//wykonujemy żądanie i pobieramy zawartość strony jako string
string pageContent = client.DownloadString("https://www.example.com");

//wyświetlamy pobraną zawartość w konsoli
Console.WriteLine(pageContent);
```
Ten prosty kod pobierze zawartość strony i wyświetli ją w konsoli. Możemy również zapisać pobraną zawartość do pliku lub przetworzyć ją dalej, na przykład wyodrębnić konkretne dane.

## Głębsza analiza

W powyższym przykładzie użyliśmy metody DownloadString, która pobiera zawartość strony jako string. Jednak w przypadku pobierania plików, możemy użyć metody DownloadFile, która zapisze zawartość do wskazanego pliku.

Dodatkowo, klasa WebClient umożliwia ustawianie nagłówków HTTP oraz obsługę błędów. Jest to szczególnie przydatne, gdy chcemy pobrać dane zabezpieczone uwierzytelnieniem.

## Zobacz także

- Dokumentacja klasy WebClient: https://docs.microsoft.com/pl-pl/dotnet/api/system.net.webclient
- Tutorial na temat pobierania stron internetowych w C#: https://www.c-sharpcorner.com/article/how-to-download-a-web-page-using-c-sharp

Dzięki prostym i efektywnym mechanizmom dostępnym w C#, pobieranie stron internetowych nie jest już skomplikowanym zadaniem. Teraz możesz wykorzystać tę wiedzę w swoim projekcie lub analizie danych. Powodzenia!