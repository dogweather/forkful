---
title:                "Elixir: Rozpoczynanie nowego projektu"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeżeli masz ochotę rozpocząć nowy projekt, który będzie wykorzystywał technologię Elixir, to dobrze trafiłeś! Elixir jest językiem programowania stworzonym na podstawie Erlanga, co sprawia, że jest on niezwykle wydajny i skalowalny. Ponadto, posiada również przyjazny i elastyczny składnię, co czyni go idealnym wyborem dla wielu projektów.

## Jak to zrobić

Proces tworzenia nowego projektu w Elixirze jest bardzo prosty. Wykorzystajmy do tego narzędzie o nazwie Mix. Pierwszym krokiem jest zainstalowanie Elixir oraz Mix na Twoim komputerze. Następnie, w konsoli, przejdź do folderu, w którym chcesz umieścić swój projekt i wpisz komendę `mix new nazwa_projektu`. W ten sposób Mix stworzy szkielet Twojego nowego projektu. Możesz teraz przejść do tej lokalizacji i wyświetlić zawartość katalogu, aby zobaczyć, że zostały utworzone odpowiednie pliki i foldery. 

Teraz czas na pierwszy kod w naszym projekcie. Przejdź do pliku `lib/nazwa_projektu.ex` i spróbujmy utworzyć prostą funkcję, która wyświetli napis "Witaj, świecie!". Nasz kod będzie wyglądał następująco:

```Elixir
defmodule NazwaProjektu do
  def witaj do
    IO.puts "Witaj, świecie!"
  end
end
```

Aby uruchomić ten kod, musimy przejść do konsoli i wpisać komendę `iex -S mix`, co spowoduje uruchomienie konsoli Elixir z naszym projektem. Następnie, możemy wywołać naszą funkcję używając jej nazwy oraz operatora `.`: `NazwaProjektu.witaj`. Po wykonaniu tej komendy, powinniśmy zobaczyć napis "Witaj, świecie!" w konsoli.

## Głębszy zanurzenie

Podczas tworzenia nowego projektu w Elixirze, warto poświęcić trochę czasu na zapoznanie się z narzędziem Mix oraz strukturą projektu. Możesz również zwrócić uwagę na plik `mix.exs`, który jest plikiem konfiguracyjnym dla Mix. W nim możesz zmienić ustawienia i dodawać zależności dla swojego projektu.

Pamiętaj również, aby zapoznać się z dokumentacją Elixira oraz dostępnymi bibliotekami, które mogą ułatwić Ci pracę nad projektem.

## Zobacz również

- Dokumentacja Elixira: https://elixir-lang.org/docs.html
- Biblioteka Phoenix: https://phoenixframework.org/
- Oficjalny blog Elixira: https://elixir-lang.org/blog/