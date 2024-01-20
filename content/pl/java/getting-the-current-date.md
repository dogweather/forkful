---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pobieranie aktualnej daty w Javie: Szybki przewodnik

## Co i dlaczego?
Pobieranie aktualnej daty w Javie to rutynowe zadanie, polegające na odczytaniu bieżącej daty i czasu bezpośrednio z systemu operacyjnego. Programiści robią to po to, aby śledzić momenty wystąpienia określonych zdarzeń lub zarządzać datami ważności w aplikacji.

## Jak to zrobić:
Za pomocą poniższego kodu, mozemy pobrac aktualną datę:

```Java
import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) {
        LocalDateTime teraz = LocalDateTime.now();
        System.out.println("Aktualna data i czas: " + teraz);
    }
}
```

Po uruchomieniu powyższej klasy `Main`, wynik byłby mniej więcej taki:

```Java
Aktualna data i czas: 2022-04-16T14:15:55.354
```

## Deep Dive
Początkowo, dla celów zarządzania datami i czasem w Javie, używano klas Date i Calendar z pakietu java.util. Jednak były one kłopotliwe w obsłudze i miały wiele innych problemów, dlatego od Javy 8 zaczęto używać pakietu java.time.

Inna klasa, którą moglibyśmy użyć to `java.time.LocalDate`, która zwraca tylko datę. Stanowi to przydatną alternatywę, kiedy nie zależy nam na czasie.

A oto szczegół techniczny: kiedy używamy `LocalDateTime.now()`, domyślnie używamy zegara systemowego, co nie zawsze może być idealnym rozwiązaniem, na przykład podczas testowania.

## Zobacz też
- Więcej informacji na temat klasy LocalDateTime można znaleźć w [oficjalnej dokumentacji Oracle](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html).
- Aby dowiedzieć się więcej o zarządzaniu datami i czasem w Javie, sprawdź [przewodnik na stronie Baeldung](https://www.baeldung.com/java-8-date-time-intro).