---
title:                "Python: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele interakcji między użytkownikami a serwerami internetowymi odbywa się poprzez protokół HTTP (Hypertext Transfer Protocol). Wysyłanie żądań HTTP jest niezbędnym elementem programowania aplikacji sieciowych, ponieważ umożliwia ono pobieranie danych z serwera oraz wysyłanie informacji do niego. Dzięki temu, możemy np. przeglądać strony internetowe, logować się do naszych kont czy wysyłać wiadomości.

## Jak to zrobić

W Pythonie istnieje wiele bibliotek pozwalających na wysyłanie żądań HTTP. Jedną z najpopularniejszych jest `requests`, dlatego skupimy się na niej w tym artykule. Najpierw musimy zainstalować tę bibliotekę za pomocą narzędzia `pip`:

```Python
pip install requests
```

Następnie importujemy ją w naszym kodzie:

```Python
import requests
```

Aby wysłać żądanie HTTP do serwera, musimy utworzyć obiekt typu `Request` i przekazać do niego adres URL, do którego chcemy się połączyć. Możemy również określić niektóre opcje, np. nagłówki lub parametry żądania. W poniższym przykładzie wyślemy żądanie GET do strony "www.example.com" i wyświetlimy otrzymaną odpowiedź:

```Python
r = requests.get("https://www.example.com")
print(r.text)
```

Wynik powinien zawierać kod HTML strony www.example.com. Jeśli chcemy wysłać żądanie POST, w którym przesyłamy dane, możemy to zrobić w następujący sposób:

```Python
payload = {'key1': 'value1', 'key2': 'value2'}
r = requests.post("https://www.example.com", data=payload)
print(r.text)
```

Możemy również wykonywać inne typy żądań HTTP, takie jak PUT, DELETE czy PATCH, korzystając z odpowiednich metod. Pełna lista dostępnych metod i szczegółowe informacje znajdują się w dokumentacji biblioteki.

## Deep Dive

Większość bibliotek do obsługi żądań HTTP opiera się na wbudowanym module `urllib` lub `urllib2`. Jednak biblioteka `requests` oferuje większą prostotę i intuicyjność w korzystaniu z metod HTTP. Ponadto, umożliwia ona automatyczne obsługiwanie różnych typów kodów odpowiedzi, co znacznie ułatwia nasze zadanie. Warto również zwrócić uwagę na możliwość ustawiania i przechowywania ciasteczek, czyli danych sesji, w celu późniejszego wykorzystania ich w kolejnych żądaniach.

## Zobacz też

- Oficjalna dokumentacja biblioteki `requests`: https://requests.readthedocs.io/en/latest/
- Wprowadzenie do obsługi HTTP w Pythonie: https://realpython.com/python-requests/