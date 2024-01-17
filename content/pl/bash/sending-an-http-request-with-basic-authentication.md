---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Bash: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O co chodzi?

Wysyłanie żądania HTTP z podstawową autoryzacją jest jedną z metod uwierzytelniania w sieci. Programiści korzystają z niej, aby potwierdzić swoją tożsamość i uzyskać dostęp do chronionych zasobów.

## Jak to zrobić:

Aby wysłać żądanie HTTP z podstawową autoryzacją w Bashu, użyj polecenia `curl` z flagą `-u` i podaj login i hasło w formacie `login:hasło`. Przykładowe użycie:

```Bash
curl -u user:password http://example.com/api
```

Jeśli uwierzytelnienie przebiegnie pomyślnie, to otrzymasz odpowiedź serwera. W przeciwnym razie, dostaniesz błąd autoryzacji.

## Głębszy zanurzenie:

Podstawowa autoryzacja HTTP powstała w 1996 roku jako sposób na uwierzytelnianie użytkowników w Internecie. Jest to prosta, ale niezależna od sieci metoda, która nie wymaga żadnych dodatkowych narzędzi lub bibliotek.

Alternatywnymi metodami uwierzytelniania są m.in. uwierzytelnianie oparte o tokeny, OAuth lub SSL. Każda z nich ma swoje wady i zalety, dlatego wybór najlepszej metody zależy od konkretnego przypadku.

W Bashu, podstawowa autoryzacja jest realizowana przez protokół HTTP, który komunikuje się z serwerem z użyciem zapytań GET, POST, PUT i DELETE. Dzięki temu, programiści mogą wykorzystać ją w prosty i intuicyjny sposób.

## Zobacz również:

Jeśli chcesz dowiedzieć się więcej o podstawowej autoryzacji HTTP, to polecamy przeczytać dokumentację protokołu HTTP. Możesz także wykorzystać inne narzędzia takie jak Postman lub Insomnia do wysyłania zapytań z podstawową autoryzacją.