---
date: 2024-01-20 18:02:11.980303-07:00
description: "How to: (Jak to zrobi\u0107:) ."
lastmod: '2024-03-13T22:44:35.629572-06:00'
model: gpt-4-1106-preview
summary: .
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## How to: (Jak to zrobić:)
```PowerShell
# Przykład żądania GET z użyciem podstawowej autentykacji
$User = 'jan_kowalski'
$Password = 'tajneHaslo123'
$Pair = "$($User):$($Password)"
$EncodedCredentials = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($Pair))
$Headers = @{ Authorization = "Basic $EncodedCredentials" }

$response = Invoke-RestMethod -Uri 'https://example.com/api/data' -Method Get -Headers $Headers

# Wynik
$response | ConvertTo-Json
```

## Deep Dive (Głębsze spojrzenie):
Podstawowa autentykacja HTTP to prosty mechanizm bezpieczeństwa, znany od czasów wczesnych wersji protokołu HTTP. Zakłada ona zakodowanie nazwy użytkownika i hasła w Base64 i ich przesyłanie w nagłówku żądania.

Alternatywą jest na przykład autentykacja oparta o tokeny (np. OAuth), która jest bezpieczniejsza, ale bardziej skomplikowana w implementacji.

Ważne jest, aby pamiętać, że wykorzystanie podstawowej autentykacji bez szyfrowania połączenia (HTTPS) jest niebezpieczne i naraża dane na przechwycenie.

## See Also (Zobacz również):
- [About HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Invoke-RestMethod documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
