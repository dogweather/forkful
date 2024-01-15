---
title:                "Wysyłanie żądania http"
html_title:           "C#: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądań HTTP jest nieodłączną częścią programowania aplikacji internetowych. Dzięki nim możemy pobierać i wysyłać dane, dzieląc się informacjami z innymi serwerami.

## Jak to zrobić

### Przygotowanie

Aby móc wysłać żądanie HTTP w C#, musimy najpierw zaimportować przestrzeń nazw ```System.Net.Http```. Możesz to zrobić, dodając na początku swojego pliku ```using System.Net.Http;```

### Wysyłanie żądania GET

Aby wysłać żądanie GET do określonego adresu URL, możemy skorzystać z klasy ```HttpClient``` oraz metody ```GetAsync```. Poniżej znajduje się przykładowy kod, który wyśle żądanie GET do strony "https://example.com" oraz wyświetli jego zawartość w konsoli.

```C#
var httpClient = new HttpClient();
var response = await httpClient.GetAsync("https://example.com");
var content = await response.Content.ReadAsStringAsync();

Console.WriteLine(content);
```

Przykładowy output:

```
<!doctype html>
<html>
<head>
<title>Example Domain</title>

<meta charset="utf-8" />
<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<style type="text/css">
body {
background-color: #f0f0f2;
margin: 0;
padding: 0;
font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
...

</body>
</html>
```

### Wysyłanie żądania POST

Aby wysłać żądanie POST, musimy najpierw utworzyć obiekt typu ```HttpRequestMessage```, a następnie przekazać go jako parametr do metody ```PostAsync```. Przykładowy kod poniżej przedstawia wysłanie żądania z danymi w formacie JSON do strony "https://example.com".

```C#
var httpClient = new HttpClient();

var data = new { name = "John", age = 30 };
var content = new StringContent(JsonConvert.SerializeObject(data), Encoding.UTF8, "application/json");

var response = await httpClient.PostAsync("https://example.com", content);
var result = await response.Content.ReadAsStringAsync();

Console.WriteLine(result);
```

### Obsługa wyjątków

Podczas wysyłania żądań HTTP, możemy napotkać różne problemy, na przykład brak połączenia z serwerem. W takich przypadkach, warto zapewnić obsługę wyjątków, aby odpowiednio reagować na błędy lub je zignorować.

```C#
try
{
    // try sending the request
    var response = await httpClient.GetAsync("https://example.com");
	var content = await response.Content.ReadAsStringAsync();

	Console.WriteLine(content);
}
catch(Exception ex)
{
    Console.WriteLine("There was an error: " + ex.Message);
}
```

## Deep Dive

Głębsze zanurzenie w temat wysyłania żądań HTTP wymaga wiedzy na temat różnych metod, nagłówków oraz statusów odpowiedzi HTTP. Warto również zapoznać się ze specyfikacją protokołu HTTP/1.1, aby lepiej zrozumieć proces przesyłania danych w sieci.

## Zobacz również

- [Dokumentacja klasy HttpClient](https://docs.microsoft.com/pl-pl/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Lista HTTP statusów odpowiedzi](https://developer.mozilla.org/pl/docs/Web/HTTP/Status)