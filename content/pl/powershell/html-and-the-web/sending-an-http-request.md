---
title:                "Wysyłanie żądania HTTP"
aliases:
- /pl/powershell/sending-an-http-request/
date:                  2024-01-20T18:01:23.739316-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Wysyłamy zapytanie HTTP, by porozumieć się z serwisami internetowymi. Programiści robią to, aby pobierać dane, wysyłać informacje, autoryzować użytkowników, i więcej.

## How to: (Jak to zrobić?)
Wysłanie prostej prośby GET do serwisu:

```PowerShell
$response = Invoke-RestMethod -Uri 'https://jsonplaceholder.typicode.com/todos/1' -Method Get
$response
```

Przykładowy wynik:

```
userId    : 1
id        : 1
title     : delectus aut autem
completed : false
```

Wysyłanie danych metodą POST:

```PowerShell
$uri = 'https://jsonplaceholder.typicode.com/posts'
$body = @{
  title = 'foo'
  body = 'bar'
  userId = 1
}
$response = Invoke-RestMethod -Uri $uri -Method Post -Body ($body | ConvertTo-Json) -ContentType 'application/json'
$response
```

Przykładowy wynik:

```
userId : 1
id     : 101
title  : foo
body   : bar
```

## Deep Dive (Dogłębna analiza)
Invoke-RestMethod to dość nowe rozwiązanie w PowerShellu. Przed jego pojawieniem się, zadanie wykonałbyś z WebClient lub HttpWebRequest. Świat się zmienia, HTTP/2 i HTTP/3 wkraczają, lecz Invoke-RestMethod jest wystarczająco elastyczny, by podążać za tym. 

Alternatywy, jak cURL czy POSTMAN, są dobre do testowania, ale nie zawsze wygodne w skryptach. Invoke-RestMethod może też używać autoryzacji, nagłówków HTTP, a nawet obsługiwać sesje związane z ciasteczkami.

Co więcej, obsługa błędów: jeżeli zapytanie się nie powiedzie, Invoke-RestMethod może zwrócić wyjątek z HttpErrorResponseException zawierający status odpowiedzi oraz treść błędu.

## See Also (Zobacz również)
- [Invoke-RestMethod Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [About HTTP Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Understanding REST APIs](https://www.smashingmagazine.com/2018/01/understanding-using-rest-api/)
- [PowerShell Gallery for REST-related Modules](https://www.powershellgallery.com/)
