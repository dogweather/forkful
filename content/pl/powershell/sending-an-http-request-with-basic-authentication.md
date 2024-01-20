---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem polega na przesyłaniu nazwy użytkownika i hasła w formie zakodowanej za pomocą base64 w nagłówku `Authorization` żądania HTTP. Programiści robią to, aby uzyskać dostęp do chronionych zasobów na serwerze.

## Jak to zrobić:

Aby wysłać żądanie HTTP z podstawowym uwierzytelnianiem w PowerShell, możemy skorzystać z polecenia `Invoke-RestMethod`. W poniższym przykładzie wysyłamy żądanie GET na adres URL `https://api.github.com`.

```PowerShell
$User = 'Username'
$Pass = 'Password'

$pair = "$($User):$($Pass)"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))

$basicAuthValue = "Basic $encodedCreds"
$Headers = @{
    Authorization = $basicAuthValue
}

$response = Invoke-RestMethod -Uri 'https://api.github.com' -Method Get -Headers $Headers
```
A oto jak wygląda przykładowy wynik:

```PowerShell
$current_rate_limit = $response.rate.limit
write-host "Current rate limit: $current_rate_limit"
```

## Głębszy Wgląd

Kiedy wysyłamy żądanie HTTP z podstawowym uwierzytelnianiem, jesteśmy odpowiedzialni za przekazanie naszych danych uwierzytelniających serwerowi. Powstało to w latach 90-tych i jestprostą metoda uwierzytelniania, ale nie jest idealnym rozwiązaniem, jeśli chodzi o bezpieczeństwo.

Alternatywą jest korzystanie z uwierzytelniania typu `Bearer`, które jest bardziej bezpieczne, albo `Digest`, które wymaga więcej wymian między klientem a serwerem.

Detale implementacyjne zależą przede wszystkim od serwisu, z którym pracujemy. W przypadku GitHuba, na przykład, musisz przekazać swoje uwierzytelnianie tak, jak to zrobiliśmy w powyższym przykładzie, inaczej dostaniemy błąd.

## Zobacz Też

- [Dokumentacja PowerShell Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)
- [RFC 7617](https://tools.ietf.org/html/rfc7617) (The 'Basic' HTTP Authentication Scheme)
- [Dokumentacja GitHub API](https://docs.github.com/en/rest/guides/getting-started-with-the-rest-api)