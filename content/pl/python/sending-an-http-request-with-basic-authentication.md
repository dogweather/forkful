---
title:                "Python: Wysyłanie żądania http z uwierzytelnieniem podstawowym"
simple_title:         "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach coraz więcej aplikacji internetowych wymaga uwierzytelnienia użytkownika. Aby to osiągnąć, programiści muszą wykorzystać standardowy protokół HTTP wraz z dodatkowym mechanizmem uwierzytelnienia. W tym artykule dowiesz się, jak używać podstawowej autoryzacji HTTP w języku Python, aby przesyłać zapytania do serwera WWW.

## Jak to zrobić

Aby wysłać żądanie HTTP z podstawową autoryzacją w Pythonie, musisz wykonać trzy kroki.

1. Importuj bibliotekę `requests`:
```Python
import requests
```

2. Utwórz obiekt z informacjami o uwierzytelnieniu, które zostaną dodane do nagłówka żądania HTTP:
```Python
auth = HTTPBasicAuth('użytkownik', 'hasło')
```

3. Wyślij żądanie HTTP do określonego adresu URL przy użyciu metody `get` lub `post` i przekazuj obiekt autoryzacji jako parametr `auth`:
```Python
response = requests.get('http://example.com', auth=auth)
```

Oto przykładowy kod, który wysyła żądanie GET do serwera, który wymaga podstawowej autoryzacji i wypisuje odpowiedź na konsolę:
```Python
import requests

auth = HTTPBasicAuth('użytkownik', 'hasło')
response = requests.get('http://example.com', auth=auth)
print(response.text)
```

Po uruchomieniu tego kodu otrzymamy odpowiedź serwera, która może wyglądać mniej więcej tak:
```
<!DOCTYPE html>
<html>
<head>
    <title>Strona przykładowa</title>
</head>
<body>
    <p>Witaj, użytkowniku!</p>
</body>
</html>
```

## Deep Dive

Aby lepiej zrozumieć działanie podstawowej autoryzacji HTTP w języku Python, warto przyjrzeć się jej dokładniej. W przypadku uwierzytelnienia podstawowego, nazwa użytkownika i hasło są przesyłane w nagłówku żądania z kodowaniem w formacie Base64. Należy pamiętać, że takie uwierzytelnianie nie jest bezpieczne i może być łatwo złamane. Dlatego też zaleca się używanie bardziej zaawansowanych metod, takich jak uwierzytelnianie z użyciem tokenów lub kluczy API.

## Zobacz również

- [Dokumentacja biblioteki requests](https://requests.readthedocs.io/en/master/)
- [Oficjalny poradnik Pythona dla początkujących](https://docs.python.org/3/tutorial/index.html)
- [Artykuł na temat bezpiecznego uwierzytelniania w sieci](https://lwn.net/Articles/784193/)