---
title:                "Wysyłanie żądania http"
html_title:           "Python: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP jest nieodłączną częścią wielu projektów programistycznych. Za pomocą tego prostego zadania możemy pobierać dane, komunikować się z serwerami i tworzyć dynamiczne aplikacje internetowe.

## Jak to zrobić

Wysłanie żądania HTTP w Pythonie jest łatwe przy użyciu biblioteki `requests`. Najpierw musimy ją zainstalować za pomocą `pip install requests`, a następnie zaimportować w naszym kodzie:

```Python
import requests
```

Aby wysłać żądanie, musimy określić adres URL, do którego chcemy się połączyć, oraz metodę żądania. Na przykład, jeśli chcemy pobrać stronę internetową, musimy użyć metody `GET`:

```Python
response = requests.get("https://www.google.com")
```

Teraz `response` będzie zawierać obiekt odpowiedzi z serwera. Możemy sprawdzić status naszego żądania za pomocą atrybutu `status_code`:

```Python
print(response.status_code)
# output: 200
```

Jeśli chcemy uzyskać zawartość strony, możemy użyć atrybutu `text`:

```Python
print(response.text)
# output: zalany kod HTML strony Google
```

## Deep Dive

Podczas wysyłania żądań HTTP istnieje wiele innych opcji, które możemy wykorzystać, takich jak przekazywanie parametrów, nagłówków czy danych formularza. Możemy również przeprowadzać bardziej zaawansowane operacje, jak np. wysyłanie żądania POST lub przesyłanie plików.

Aby dowiedzieć się więcej o możliwościach biblioteki `requests`, warto zapoznać się z jej dokumentacją: https://docs.python-requests.org/en/master/.

## Zobacz także

- Tutorial dotyczący biblioteki `requests`: https://realpython.com/python-requests/
- Poradnik dotyczący wysyłania żądań HTTP w Pythonie: https://code.tutsplus.com/pl/tutorials/http-in-python-from-begginers--net-30333