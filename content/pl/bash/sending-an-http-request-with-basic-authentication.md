---
title:                "Wysyłanie żądania HTTP z podstawową autoryzacją"
html_title:           "Bash: Wysyłanie żądania HTTP z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania HTTP z podstawową autoryzacją"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Dlaczego

W dzisiejszym artykule dowiesz się, dlaczego przesyłanie żądań HTTP z podstawową autoryzacją jest ważnym narzędziem dla programistów i jak można to łatwo osiągnąć w Bash.

# Jak to zrobić

W Bash istnieje wiele sposobów na wysłanie żądania HTTP z podstawową autoryzacją, ale przedstawimy tutaj najprostszy i najbardziej przejrzysty sposób. W pierwszej kolejności należy ustawić zmienne z nazwą użytkownika i hasłem, a następnie wykonać polecenie ```curl``` z odpowiednimi flagami. Oto przykładowy kod:

```Bash
username='example'
password='password123'

curl -u $username:$password http://example.com
```

W powyższym przykładzie, zmienna ```$username``` przechowuje nazwę użytkownika, a zmienna ```$password``` przechowuje hasło. Następnie, używając flagi ```-u```, możemy przekazać te zmienne do polecenia ```curl```, razem z adresem URL, na który chcemy wysłać żądanie.

Po wykonaniu powyższego polecenia, w konsoli powinien pojawić się wynik tego żądania, w tym również odpowiedź serwera.

# Deep Dive

Podstawowa autoryzacja jest jednym z najprostszych sposobów na zabezpieczenie żądań HTTP. Polega ona na przekazywaniu nazwy użytkownika i hasła w nagłówku ```Authorization```. Serwer porównuje te dane z danymi znajdującymi się w jego bazie danych i jeśli się zgadzają, udziela dostępu do żądanego zasobu.

W przypadku wysyłania żądania z podstawową autoryzacją w Bash, polecenie ```curl``` po prostu generuje odpowiedni nagłówek i przekazuje go wraz z żądaniem. Istnieje również możliwość przekazania danych w sposób bezpieczniejszy, poprzez użycie pliku zawierającego nazwę użytkownika i hasło, a następnie wykorzystanie flagi ```--netrc```.

Warto również wspomnieć, że podstawowa autoryzacja nie jest zalecana do użytku w bezpiecznych systemach, ponieważ dane uwierzytelniające są przekazywane jako tekst jawny i mogą być wyłapane przez niepożądane osoby. W takich sytuacjach lepiej użyć bardziej zaawansowanych metod autoryzacji, takich jak np. OAuth.

# Zobacz również

- Dokumentacja cURL: https://curl.haxx.se/docs/manpage.html
- Artykuł na temat podstawowej autoryzacji: https://developer.mozilla.org/pl/docs/Web/HTTP/Authentication
- Przykładowe użycie autoryzacji w Bash: https://stackoverflow.com/questions/30089327/pass-username-password-to-curl-in-bash