---
title:    "Elixir: Pobieranie bieżącej daty"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Dlaczego warto pobierać bieżącą datę w Elixirze? 

Pobieranie bieżącej daty może być bardzo przydatne w wielu projektach Elixir. Może to pomóc w obliczaniu czasu wykonywania zadania, śledzeniu interakcji z bazą danych, a także w synchronizacji aplikacji z zewnętrznymi źródłami. Dzięki temu artykułowi dowiesz się, jak łatwo i szybko uzyskać bieżącą datę w Elixirze. 

## Jak to zrobić

Pierwszą i najprostszą metodą na pobranie bieżącej daty jest użycie funkcji `DateTime.utc_now/0`. Przykładowy kod wyglądałby następująco: 

```Elixir 
DateTime.utc_now() 
|> DateTime.to_string() 
```

Wynikiem będzie bieżąca data i godzina w formacie "YYYY-MM-DD HH:MM:SS.SSSSSS".

Można również skorzystać z funkcji `DateTime.from_unix/1`, która pobiera miesiąc, dzień, rok i godzinę w formie unixtimestamp. W poniższym przykładzie, użyty zostaje obecny czas w unixtimestamp, który następnie jest przekształcany na format daty. 

```Elixir 
DateTime.from_unix(System.system_time(:seconds)) 
|> DateTime.to_string() 
```

## Głębszy wgląd

Podczas pobierania bieżącej daty, warto także wziąć pod uwagę różnice w strefach czasowych. Można to zrobić dzięki funkcji `DateTime.to_timestamp/2`, która pozwala na przekształcenie daty do czasu lokalnego lub innego ważnego dla nas miejsca. Przykładowy kod wyglądałby następująco:

```Elixir 
DateTime.utc_now() 
|> DateTime.to_timestamp("Europe/Warsaw") 
``` 

Output będzie datą i godziną w strefie czasowej Europy/Warszawy. 

# Zobacz także 

1. Dokumentacja Elixir: https://hexdocs.pm/elixir/ 
2. Funkcje DataTime: https://hexdocs.pm/elixir/DateTime.html 
3. Przekształcanie dat i czasu: https://elixirschool.com/en/lessons/advanced/datetime/