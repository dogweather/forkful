---
title:                "Wysyłanie żądania HTTP"
aliases:
- pl/python/sending-an-http-request.md
date:                  2024-01-20T18:00:17.565958-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysłanie żądania HTTP (Hypertext Transfer Protocol) to sposób, w jaki nasz program może komunikować się z serwerami w Internecie. Robimy to, żeby pobrać dane, wysłać informacje, autoryzować użytkowników i wiele więcej – to podstawa interakcji w sieci.

## Jak to zrobić:

Użyjemy `requests`, łatwej w użyciu biblioteki pozwalającej na wysyłanie żądań HTTP. Instalacja to linijka w terminalu:

```Python
pip install requests
```

A oto prosty przykład użycia:

```Python
import requests

response = requests.get('https://api.github.com')
print(response.status_code)
print(response.content)
```

Output może być taki:

```Python
200
b'{"current_user_url":"https://api.github.com/user","current_user_authorizations_html_url":"https://github.com/settings/connections/applications{/client_id}",...}'
```

Kod `200` mówi nam, że żądanie się powiodło.

## Deep Dive

Protokół HTTP istnieje od 1991 roku. Z czasem ewoluował – mamy już HTTP/2 i eksperymentujemy z HTTP/3. Alternatywą dla biblioteki `requests` może być `http.client` wbudowany w Pythona, choć nie jest tak wygodny w użyciu.

Biblioteka `requests` obsługuje sesje, co pozwala na przechowywanie ciasteczek czy utrzymywanie stałego nagłówka przez wiele żądań. Wsparcie dla HTTPS jest wbudowane, a co za tym idzie, szyfrowanie danych również. Kiedy wysyłasz żądanie, `requests` automatycznie koduje parametry, a odpowiedzi mogą być łatwo przekształcone w struktury danych JSON.

## Zobacz również

- [Dokumentacja `requests`](https://requests.readthedocs.io/en/master/)
- [Przewodnik po żądaniach HTTP w Pythonie](https://realpython.com/python-requests/)
- [HTTP/3 Explained](https://http3-explained.haxx.se/en/)
- [Dokumentacja `http.client`](https://docs.python.org/3/library/http.client.html)

Pamiętaj, że praktyka czyni mistrza – eksperymentuj z wysyłaniem różnych typów żądań i badaniem odpowiedzi. W sieci można znaleźć mnóstwo API do wypróbowania. Powodzenia!
