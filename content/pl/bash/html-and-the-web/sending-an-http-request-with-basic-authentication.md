---
date: 2024-01-20 18:01:12.472873-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawowym uwierzytelnieniem\
  \ to proces, gdzie przekazujesz swoje dane logowania w formie nag\u0142\xF3wka HTTP,\
  \ aby uzyska\u0107 dost\u0119p do\u2026"
lastmod: '2024-03-13T22:44:35.584705-06:00'
model: gpt-4-1106-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawowym uwierzytelnieniem to\
  \ proces, gdzie przekazujesz swoje dane logowania w formie nag\u0142\xF3wka HTTP,\
  \ aby uzyska\u0107 dost\u0119p do\u2026"
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## Co i Dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to proces, gdzie przekazujesz swoje dane logowania w formie nagłówka HTTP, aby uzyskać dostęp do zabezpieczonych zasobów. Programiści używają tej metody do komunikacji z API, które wymaga autoryzacji.

## Jak to zrobić:
Będziesz potrzebować `curl`, narzędzia dostępnego w większości dystrybucji Linuxa. Oto jak to zrobić:

```Bash
# Format podstawowy
curl -u username:password URL

# Przykład
curl -u jan:kowalski http://example.com/data

# Do przesyłania danych (POST request) użyj flagi -d
curl -u jan:kowalski -d "param1=value1&param2=value2" -X POST http://example.com/submit
```

Jeżeli zależy Ci na bezpieczeństwie, nie umieszczaj haseł bezpośrednio w poleceniach. Zamiast tego, użyj `-u username:`, a `curl` poprosi o hasło.

```Bash
curl -u jan: http://example.com/data
Enter host password for user 'jan':
```

Po wykonaniu komendy zobaczysz odpowiedź serwera - dane lub komunikat o błędzie.

## Głębsze zanurzenie
Podstawowe uwierzytelnienie HTTP to stary i prosty sposób na ochronę zasobów sieciowych. Do pary login-hasło dodaje się `Authorization` w nagłówku żądania HTTP, używając kodowania Base64. Ważne: to nie jest metoda szczególnie bezpieczna, ponieważ te dane mogą być łatwo zdekodowane. Używaj HTTPS, aby zaszyfrować całą komunikację.

Alternatywą jest uwierzytelnienie oparte na tokenach, na przykład OAuth 2.0, które zapewnia większe bezpieczeństwo. W tym przypadku, używasz tokenu dostępu zamiast par login-hasło. Token jest generowany przez serwer autoryzacyjny i często ma ograniczony czas życia.

W historii programowania, ewolucja uwierzytelnienia odzwierciedla wzrost znaczenia bezpieczeństwa i prywatności. Z każdym nowym mechanizmem, jak OAuth, JWT, lub klucze API, idzie też wzrost złożoności. Jednakże, podstawowe uwierzytelnienie wciąż ma swoje miejsce do szybkiego prototypowania lub wewnętrznych, bezpiecznych sieciach.

## Zobacz też:
- Dokumentacja `curl`: https://curl.se/docs/manpage.html
- Więcej o podstawowym uwierzytelnieniu w HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Bezpieczeństwo uwierzytelnienia HTTP: https://owasp.org/www-community/controls/Basic_Authentication
- Wprowadzenie do OAuth 2.0: https://oauth.net/2/
- Przewodnik dla początkujących po HTTPS: https://www.cloudflare.com/learning/ssl/what-is-https/
