---
date: 2024-01-20 18:01:50.556483-07:00
description: "Jak to zrobi\u0107: Podstawowe uwierzytelnienie (Basic Authentication)\
  \ to stara metoda ochrony dost\u0119pu do webowych zasob\xF3w. U\u017Cytkownik przesy\u0142\
  a nazw\u0119 i has\u0142o w\u2026"
lastmod: '2024-04-05T21:53:37.268150-06:00'
model: gpt-4-1106-preview
summary: "Podstawowe uwierzytelnienie (Basic Authentication) to stara metoda ochrony\
  \ dost\u0119pu do webowych zasob\xF3w."
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

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
