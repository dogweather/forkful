---
title:                "Fish Shell: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli tworzysz aplikację internetową lub program komputerowy, który musi komunikować się z zewnętrznymi serwisami, często będziesz potrzebować wysyłania żądań HTTP. W takich sytuacjach, najważniejsze jest zapewnienie bezpieczeństwa i uwierzytelnienia danych. W tym artykule dowiesz się, dlaczego warto używać prostego uwierzytelniania w żądaniach HTTP.

## Jak to zrobić

Jeśli używasz Fish Shell, możesz łatwo wysyłać żądania HTTP z uwierzytelnieniem za pomocą jednego polecenia. Pierwszym krokiem jest użycie polecenia `curl` z flagą `-u`, która umożliwia dodanie nazwy użytkownika i hasła do żądania. Na przykład:

```Fish Shell
curl -u username:password https://example.com/api/endpoint
```

Jeśli uwierzytelnienie zostanie zaakceptowane, otrzymasz odpowiedź ze strony internetowej lub serwera zgodną z dokumentacją API. Jeśli jednak zostaniesz odrzucony, oznacza to, że nazwa użytkownika lub hasło są nieprawidłowe.

## Głębokie zanurzenie

Gdy wysyłasz żądanie z uwierzytelnieniem, niepowodzenie może być spowodowane przez różne problemy, takie jak błędne dane, przekroczenie limitu czasu lub problem z serwerem. W przypadku problemów z uwierzytelnieniem, warto sprawdzić, czy dane są poprawne i czy strona internetowa lub serwer są prawidłowo skonfigurowane. Jeśli masz problemy z uwierzytelnieniem, możesz użyć narzędzi do debugowania, takich jak `curl --trace-ascii` lub pluginów Fish Shell, które pozwolą na dokładniejsze zdiagnozowanie problemu.

## Zobacz także

- [Dokumentacja Fish Shell do polecenia curl](https://fishshell.com/docs/current/commands.html#curl)
- [Dokumentacja API z przykładami wykorzystania uwierzytelnienia](https://example.com/api/docs)
- [Poradnik programowania w Fish Shell](https://fishshell.com/docs/current/tutorial.html)