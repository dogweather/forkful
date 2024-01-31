---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
date:                  2024-01-20T18:01:50.556483-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem umożliwia dostęp do zabezpieczonych zasobów sieciowych. Programiści korzystają z tego mechanizmu, aby bezpiecznie wymieniać dane między klientem a serwerem.

## Jak to zrobić:
```Fish Shell
# Ustawiamy zmienne dla użytkownika i hasła
set USER "moj_uzytkownik"
set PASSWORD "moje_haslo"

# Kodujemy poświadczenia do formatu Base64
set ENCODED_CREDENTIALS (echo -n "$USER:$PASSWORD" | base64)

# Wysyłamy żądanie GET z nagłówkiem autoryzacyjnym
curl -H "Authorization: Basic $ENCODED_CREDENTIALS" "http://twojserwer.com/zabezpieczona_strona"

# Oczekiwany wynik: Odpowiedź serwera (HTML, JSON, itd.) lub komunikat o błędzie 
```

## Deep Dive
Podstawowe uwierzytelnienie (Basic Authentication) to stara metoda ochrony dostępu do webowych zasobów. Użytkownik przesyła nazwę i hasło w nagłówku żądania, zakodowane w base64. Chociaż prostota metody ma swoje zalety, nie jest ona najbezpieczniejsza; informacje przesyłane są bez szyfrowania, mogą więc zostać przechwycone. Zalecane jest stosowanie HTTPS, które zapewnia szyfrowanie połączenia. Istnieją również inne, bardziej bezpieczne metody uwierzytelnienia, takie jak OAuth.

Implementacja w Fish jest prosta. Korzystamy z wbudowanego programu `curl`, który obsługuje żądania HTTP. Dane poświadczeń muszą być odpowiednio zakodowane. `echo -n` zapobiega dodawaniu znaku nowej linii do zakodowanego ciągu, a `base64` robi właściwą konwersję. 

## See Also
- Dokumentacja Fish Shell: https://fishshell.com/docs/current/index.html
- Dokumentacja `curl`: https://curl.se/docs/
- Podstawy Basic Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Informacje o Base64: https://base64.guru/learners-guide/what-is-base64
