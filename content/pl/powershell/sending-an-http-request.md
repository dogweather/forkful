---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP to proces, w którym twój program komunikuje się z serwerem internetowym. Programiści robią to, aby uzyskać dane, wymienić informacje lub interaktywnie pracować z usługami sieciowymi.

## Jak to zrobić:

PowerShell sprawia, że wysyłanie żądań HTTP jest proste jak bułka z masłem. Tutaj znajduje się prosty kod, który wysyła żądanie GET do strony internetowej:

```PowerShell
$Response = Invoke-RestMethod -Uri "http://api.poznajswiat.pl/"
Write-Output $Response
```

W tym przykładzie, `Invoke-RestMethod` wysyła żądanie do podanego URI i zwraca odpowiedź. `Write-Output` wyświetla odpowiedź.

## Pogłębione informacje:

1. **Kontekst historyczny**: Pierwsze żądania HTTP wysyłane były za pomocą surowego tekstu i socketów sieciowych. Dzięki rozwojowi języków i narzędzi programistycznych, teraz jest to znacznie prostsze.

2. **Alternatywy**: W PowerShell możemy również korzystać z cmdletu `Invoke-WebRequest`, który dostarcza informacji o odpowiedzi HTTP, takich jak status czy metadane.   

3. **Detale implementacji**: Żądanie HTTP wysyłane przez `Invoke-RestMethod` jest typem "GET" domyślnie, ale można wysyłać inne typy żądań, np. "POST" lub "DELETE". Wystarczy dodać parametr `-Method`.

```PowerShell
$Response = Invoke-RestMethod -Uri "http://api.poznajswiat.pl/" -Method 'POST'
Write-Output $Response
```

## Zobacz też:

1. **Dokumentacja Microsoftu na temat Invoke-RestMethod**: [Link](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)
2. **Dokumentacja Microsoftu na temat Invoke-WebRequest**: [Link](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
3. **Składnia żądań HTTP**: [Link](https://developer.mozilla.org/pl/docs/Web/HTTP/Methods)