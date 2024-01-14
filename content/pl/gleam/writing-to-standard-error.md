---
title:    "Gleam: Pisanie do standardowego wyjścia błędów"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto pisać do standardowego błędu przy programowaniu w Gleam? To proste - ponieważ standardowy błąd (ang. standard error) jest jednym z najważniejszych sposobów wyświetlania błędów i komunikatów diagnostycznych w programach. Bez tego narzędzia trudniej byłoby nam znajdować i naprawiać błędy w naszym kodzie.

## Jak to zrobić

Aby napisać do standardowego błędu w programie w Gleam, wystarczy użyć funkcji `log.error` i przekazać do niej wiadomość lub zmienną, którą chcemy wyświetlić. Przykładowy kod może wyglądać następująco:

```Gleam
log.error("Witaj, to jest wiadomość błędu!")
```

Po uruchomieniu tego kodu, w konsoli powinien pojawić się komunikat "Witaj, to jest wiadomość błędu!". Możemy także przekazać więcej niż jedną wiadomość lub użyć zmiennych, jak w poniższym przykładzie:

```Gleam
let liczba = 5
let błąd = "nieprawidłowa wartość"

log.error("Błąd! Liczba ", liczba, " jest ", błąd, ".")
```

W efekcie otrzymamy komunikat "Błąd! Liczba 5 jest nieprawidłowa wartość.".

## Głębsze zagadnienia

Pisanie do standardowego błędu to jednak nie tylko prosty sposób na wyświetlanie komunikatów. Możemy także kontrolować, które informacje wyświetlamy, używając różnych funkcji z poziomem logowania. Na przykład, funkcja `log.error` wyświetla komunikaty tylko w przypadku błędu, natomiast funkcja `log.info` może służyć do wyświetlania ważnych informacji dla użytkownika.

Możliwości jest wiele, a dokładne poznanie wszystkich funkcji związanych z wyświetlaniem błędów i komunikatów jest kluczowe do efektywnego debugowania aplikacji w Gleam.

## Zobacz także

- Dokumentacja Gleam dotycząca wyświetlania błędów: https://gleam.run/book/tour/logging.html
- Przykłady użycia logów w Gleam: https://github.com/search?q=language%3Agleam+logging&type=Repositories
- Wiedza na temat używania standardowego błędu w innych językach programowania: https://www.digitalocean.com/community/tutorials/how-to-use-the-standard-error-stream-to-handle-errors-in-bash