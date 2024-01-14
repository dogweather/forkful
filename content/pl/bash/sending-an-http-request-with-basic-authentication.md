---
title:                "Bash: Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele aplikacji i stron internetowych wymaga uwierzytelnienia, czyli potwierdzenia tożsamości użytkownika. Jedną z najpopularniejszych metod uwierzytelniania jest autoryzacja za pomocą podstawowych danych, takich jak nazwa użytkownika i hasło. W tym artykule dowiecie się, jak wysłać zapytanie HTTP z autoryzacją za pomocą podstawowych danych w języku Bash.

## Jak to zrobić

Wysyłanie zapytań HTTP z autoryzacją za pomocą podstawowych danych może być przydatne w wielu sytuacjach, na przykład przy pobieraniu plików z serwerów lub komunikacji z API. W poniższych przykładach przedstawiamy dwa sposoby, jak to można zrobić w Bash.

### Przy użyciu polecenia curl

Polecenie `curl` to narzędzie linii poleceń służące do przesyłania danych przez protokół HTTP. Aby wysłać zapytanie z autoryzacją za pomocą podstawowych danych, wystarczy dodać flagę `-u` i podać nazwę użytkownika oraz hasło.

```Bash
curl -u username:password http://www.example.com/api/users
```

W powyższym przykładzie pobieramy dane z API użytkowników i przesyłamy autoryzację za pomocą nazwy użytkownika "username" i hasła "password". Jeśli uwierzytelnienie jest poprawne, zostanie zwrócona odpowiedź z danymi. W innym przypadku otrzymamy błąd 401 "Unauthorized" lub 403 "Forbidden".

### Przy użyciu polecenia wget

Drugim sposobem na wysłanie zapytania HTTP z autoryzacją za pomocą podstawowych danych jest użycie polecenia `wget`. W tym przypadku należy użyć flagi `--user` i `--password`.

```Bash
wget --user=username --password=password http://www.example.com/api/users
```

Podobnie jak w przypadku polecenia `curl`, jeśli autoryzacja jest poprawna, otrzymamy odpowiedź z danymi. W przypadku błędu otrzymamy komunikat o niepoprawnym uwierzytelnieniu.

## Głębsza analiza

Wysyłanie zapytań HTTP z autoryzacją za pomocą podstawowych danych jest prostym, ale ważnym sposobem na zabezpieczenie danych przesyłanych między serwerami. Dzięki temu, że stosujemy autoryzację, tylko uprawnione osoby mogą uzyskać dostęp do danych lub udostępniać je innym. Dodatkowo, każde zapytanie jest szyfrowane, co minimalizuje ryzyko przechwycenia poufnych danych.

## Zobacz też

- [Dokumentacja curl](https://curl.se/docs/manual.html)
- [Dokumentacja wget](https://www.gnu.org/software/wget/manual/wget.html)
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)