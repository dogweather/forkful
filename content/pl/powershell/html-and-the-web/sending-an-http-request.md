---
date: 2024-01-20 18:01:23.739316-07:00
description: "Wysy\u0142amy zapytanie HTTP, by porozumie\u0107 si\u0119 z serwisami\
  \ internetowymi. Programi\u015Bci robi\u0105 to, aby pobiera\u0107 dane, wysy\u0142\
  a\u0107 informacje, autoryzowa\u0107\u2026"
lastmod: '2024-02-25T18:49:33.995721-07:00'
model: gpt-4-1106-preview
summary: "Wysy\u0142amy zapytanie HTTP, by porozumie\u0107 si\u0119 z serwisami internetowymi.\
  \ Programi\u015Bci robi\u0105 to, aby pobiera\u0107 dane, wysy\u0142a\u0107 informacje,\
  \ autoryzowa\u0107\u2026"
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
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
