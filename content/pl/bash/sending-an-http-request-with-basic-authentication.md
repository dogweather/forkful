---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawową autoryzacją polega na przesłaniu danych autoryzacyjnych (loginu i hasła) w nagłówku żądania HTTP. Programiści używają tego do ochrony danych przesyłanych między klientem a serwerem.

## Jak to zrobić:

W Bashu możemy użyć narzędzia cURL do wysyłania żądań HTTP. Tu jest przykład:

```Bash
username="uzytkownik"
password="haslo"
url="https://twojastrona.pl"

curl -u $username:$password $url
```

Po wykonaniu powyższego kodu, naszemu serwerowi zostanie wysłane żądanie GET z podstawową autoryzacją.

## Głębsze spojrzenie

Podstawowa autoryzacja była częścią specyfikacji protokołu HTTP od początku. Jest prosta, ale nie zawsze jest najbezpieczniejsza - szczególnie jeśli przesyłane dane nie są zaszyfrowane.

Alternatywą jest użycie bardziej zaawansowanych form autoryzacji, takich jak OAuth lub tokeny JWT. Są one często stosowane w nowoczesnych aplikacjach webowych i API.

Podstawowa autoryzacja jest implementowana wyłącznie na poziomie nagłówków HTTP - nie ma żadnej magii stojącej za tym. Po prostu kodujemy dane autoryzacyjne w formacie `username:password` za pomocą kodowania base64, a następnie dodajemy je do nagłówka `Authorization` z prefiksem `Basic `.

## Zobacz także

Dalsze zasoby dla zainteresowanych podstawową autoryzacją HTTP:

- [Podstawowa autoryzacja HTTP na MDN](https://developer.mozilla.org/pl/docs/Web/HTTP/Authentication)
- [Bezpieczeństwo podstawowej autoryzacji](https://www.owasp.org/index.php/Basic_access_authentication)
- [Alternatywny sposób autoryzacji w HTTP na wiki](https://en.wikipedia.org/wiki/Digest_access_authentication)