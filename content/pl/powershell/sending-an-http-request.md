---
title:                "Wysyłanie żądania http"
html_title:           "PowerShell: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP jest podstawowym elementem programowania, które pozwala na komunikację z serwerem internetowym. Programiści często wykonują to zadanie w celu pobrania danych lub wykonania różnych operacji za pomocą sieci internetowej.

## Jak to zrobić?

Aby wysłać żądanie HTTP w PowerShell, należy wykorzystać wbudowany moduł `Invoke-WebRequest`. Poniżej przedstawiamy kod, który pokazuje jak wysłać żądanie GET do strony google.pl i wyświetlić pobrany kod HTML:

```
$webContent = Invoke-WebRequest -Uri "https://www.google.pl"
Write-Host $webContent.Content
```

Wynik wywołania polecenia `Write-Host` wyświetli kod HTML strony głównej Google. Aby przesłać dane z formularza, należy użyć polecenia `Invoke-RestMethod`. Przykład kodu:

```
$body = @{name="John"; age=25}
Invoke-RestMethod -Uri "https://example.com/createUser" -Method Post -Body $body
```

Ten kod wyśle ciąg znaków JSON do serwera, dzięki czemu nowy użytkownik zostanie utworzony.

## Zagłębienie się

HTTP, czyli Hypertext Transfer Protocol, jest protokołem komunikacji wykorzystywanym w sieci internetowej. Powstał w 1989 roku i od tego czasu jest podstawą komunikacji w sieci. Alternatywami dla tego protokołu są m.in. FTP, SMTP lub TCP.

W PowerShell istnieją również inne polecenia służące do wysyłania żądań HTTP, takie jak `BasicAuthentication` czy `PSCredential`. Udostępniają one bardziej wyrafinowane i bezpieczne sposoby uwierzytelniania użytkowników.

## Zobacz także

[Microsoft dokumentacja do modułu Invoke-WebRequest](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)

[Podstawy protokołu HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP)