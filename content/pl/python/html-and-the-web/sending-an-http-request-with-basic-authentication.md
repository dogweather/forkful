---
date: 2024-01-20 18:02:40.849360-07:00
description: "Jak to zrobi\u0107: Wykorzystamy bibliotek\u0119 `requests` do wys\u0142\
  ania \u017C\u0105dania HTTP. Upewnij si\u0119, \u017Ce instalacja tej biblioteki\
  \ jest ju\u017C wykonana (`pip install\u2026"
lastmod: '2024-03-13T22:44:34.948930-06:00'
model: gpt-4-1106-preview
summary: "Wykorzystamy bibliotek\u0119 `requests` do wys\u0142ania \u017C\u0105dania\
  \ HTTP."
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## Jak to zrobić:
Wykorzystamy bibliotekę `requests` do wysłania żądania HTTP. Upewnij się, że instalacja tej biblioteki jest już wykonana (`pip install requests`). Oto przykład:

```python
import requests
from requests.auth import HTTPBasicAuth

# Twój URL zasobu
url = 'http://twojserwer.com/dane'

# Twoje dane do logowania
nazwa_uzytkownika = 'uzytkownik123'
haslo = 'bezpieczneHaslo'

# Wysyłamy żądanie z autentykacją podstawową
odpowiedz = requests.get(url, auth=HTTPBasicAuth(nazwa_uzytkownika, haslo))

# Sprawdzamy odpowiedź
if odpowiedz.status_code == 200:
    print('Sukces:', odpowiedz.text)
elif odpowiedz.status_code == 401:
    print('Nieudane logowanie. Sprawdź dane do autentykacji.')
else:
    print('Błąd:', odpowiedz.status_code)
```

Output, gdy logowanie jest udane:
```
Sukces: <dane zasobu>
```

Output, gdy logowanie jest nieudane:
```
Nieudane logowanie. Sprawdź dane do autentykacji.
```

## Deep Dive:
Podstawowa autentykacja HTTP jest jedną z prostszych form autentykacji, wprowadzoną w specyfikacji HTTP 1.0. Polega na wysłaniu zakodowanego w Base64 ciągu zawierającego nazwę użytkownika i hasło. To łatwe w implementacji, ale mało bezpieczne – szczególnie bez szyfrowania transportu jakim jest HTTPS.

Alternatywy to m.in. Digest Authentication, OAuth czy tokeny JWT. W przypadku wyższego poziomu bezpieczeństwa, zalecane jest użycie bardziej zaawansowanych metod.

Warto pamiętać, że niektóre biblioteki HTTP w Pythonie, jak `httpx`, ułatwiają pracę z autentykacją, oferując wbudowane mechanizmy.

## Zobacz także:
- Dokumentacja biblioteki `requests`: https://docs.python-requests.org/en/latest/
- Bezpieczne praktyki w autentykacji: https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html
- Specyfikacja HTTP Basic Authentication: https://tools.ietf.org/html/rfc7617
