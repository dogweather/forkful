---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "C#: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Przesyłanie żądania HTTP z podstawowym uwierzytelnianiem jest niezbędne, aby uzyskać dostęp do chronionych zasobów w sieci. W niektórych przypadkach, jest to również wymagane przez serwery, aby potwierdzić tożsamość użytkownika.

## Jak to zrobić

```C#
var client = new HttpClient();

// Ustawienie nagłówka uwierzytelnienia
var byteArray = Encoding.ASCII.GetBytes("username:password");
client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));

// Wysłanie żądania GET do wybranej strony
var response = await client.GetAsync("https://example.com");

// Pobranie wartości zwracanej przez serwer
var result = await response.Content.ReadAsStringAsync();
Console.WriteLine(result);
```

**Wynik:**

```
<!DOCTYPE html>
<html>
<head>
	<title>Przykładowa strona</title>
</head>
<body>
	<h1>Witaj, użytkowniku!</h1>
</body>
</html> 
```

## Głębszy wgląd

Podstawowe uwierzytelnienie jest najbardziej podstawową metodą uwierzytelniania dostępu HTTP. Polega na przesyłaniu informacji o uwierzytelnieniu w nagłówku żądania, poprzez zakodowanie loginu i hasła do postaci Base64. Serwer następnie sprawdza te dane i udziela dostępu do zasobów lub odmawia dostępu w przypadku nieprawidłowych danych uwierzytelniających.

## Zobacz też

- Dokumentacja Microsoft na temat przesyłania żądań HTTP w C#: https://docs.microsoft.com/pl-pl/dotnet/csharp/
- Tutorial na YT na temat HTTP w C#: https://www.youtube.com/watch?v=7YcW25PHnAA