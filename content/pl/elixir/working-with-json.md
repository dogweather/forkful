---
title:                "Praca z json"
html_title:           "Elixir: Praca z json"
simple_title:         "Praca z json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Elixir jest bardzo przydatnym językiem programowania, a jego wszechstronność sprawia, że jest użyteczny w różnych dziedzinach. Jedną z jego najważniejszych cech jest bardzo wydajne przetwarzanie formatu JSON. Jeśli jesteś programistą lub dopiero zaczynasz swoją przygodę z programowaniem, poznanie sposobu działania JSON w Elixir może okazać się bardzo wartościowe.

## Jak to zrobić

Aby pracować z formatem JSON w Elixir, musimy użyć modułu `Jason`, który jest dostępny w standardowej bibliotece Elixira. Najpierw należy go zainstalować za pomocą managera pakietów `mix`:

```
mix deps.get
```

Następnie musimy go zaimportować do naszego kodu:

```
import Jason
```

Teraz możemy tworzyć obiekty JSON w naszym kodzie. Na przykład, aby utworzyć tablicę z liczbami całkowitymi, możemy użyć:

```
Jason.encode!([1, 2, 3])
```

Kiedy uruchomimy ten kod, otrzymamy następujący wynik:

```
"[1, 2, 3]"
```

Możemy również kodować obiekty JSON zagnieżdżone, na przykład:

```
Jason.encode!(%{"imie": "Anna", "miasto": "Warszawa", "hobby": ["programowanie", "podróże"]})
```

To spowoduje utworzenie następującego obiektu JSON:

```
"{\"imie\": \"Anna\", \"miasto\": \"Warszawa\", \"hobby\": [\"programowanie\", \"podróże\"]}"
```

Aby odczytać dane z obiektu JSON, możemy użyć funkcji `Jason.decode!/1`. Na przykład, jeśli chcemy odczytać wartość pola `miasto` z powyższego obiektu, możemy użyć:

```
data = Jason.decode!("{\"imie\": \"Anna\", \"miasto\": \"Warszawa\", \"hobby\": [\"programowanie\", \"podróże\"]}")

data["miasto"]
```

Output:

```
"Warszawa"
```

## Głębszy wykład

Moduł `Jason` oferuje wiele innych przydatnych funkcji do pracy z JSON. Możemy na przykład formatować nasze obiekty JSON, aby były czytelniejsze, używając funkcji `Jason.format!/1`. Możemy również sprawdzić poprawność składniową naszego obiektu JSON za pomocą funkcji `Jason.validate!/1`.

Warto również zauważyć, że Elixir obsługuje rekordy i atomowe klucze w obiektach JSON, co czyni go niezwykle przyjaznym do pracy z różnymi typami danych.

## Zobacz też

- [Dokumentacja Elixir-a](https://hexdocs.pm/elixir/)
- [Dokumentacja Jason-a](https://hexdocs.pm/jason/api-reference.html)
- [Blog Elixir Polskie Stowarzyszenie](https://elixir-polska.org/blog/)