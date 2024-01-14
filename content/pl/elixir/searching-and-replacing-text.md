---
title:    "Elixir: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się nieodłączną częścią naszego życia. Wymaga nie tylko umiejętności logicznego myślenia, ale również skrupulatności i precyzji. Jednym z podstawowych zadań programisty jest manipulacja tekstem, a jednym z najważniejszych narzędzi w tym procesie jest wyszukiwanie i zamiana tekstu. Dlatego też w tym artykule opowiemy o tym, dlaczego warto się nauczyć wyszukiwania i zamiany tekstu oraz jak to zrobić w języku Elixir.

## Jak to zrobić

Wyszukiwanie i zamiana tekstu w języku Elixir może być łatwe i przyjemne, dzięki wbudowanym funkcjom biblioteki standardowej. Jedną z najczęściej używanych funkcji jest ```String.replace/4```, która pozwala na szybką i skuteczną zmianę tekstu w zadanym napisie.

Przykład użycia:
```Elixir
iex> String.replace("Witaj, świecie!", "świecie", "Polish readers")
"Witaj, Polish readers!"
```

Funkcja ```String.replace/4``` przyjmuje jako argumenty: napis, wyrażenie do znalezienia, zastępujące wyrażenie oraz opcje. Opcje te mogą być wykorzystane do zmiany sposobu wyszukiwania, na przykład przez ustawienie flagi ```global: true``` można dokonać zmiany wszystkich wystąpień danego wyrażenia.

## Głębsza analiza

Wysyzywanie i zamiana tekstu w języku Elixir nie jest ograniczone jedynie do prostej funkcji ```String.replace/4```. Istnieje wiele innych funkcji i możliwości, które warto poznać aby móc skutecznie manipulować tekstem.

Jedną z takich funkcji jest ```Regex.replace/3```, która pozwala na wykorzystanie wyrażeń regularnych do wyszukania i zamiany tekstu. Ta funkcja może być bardzo przydatna w bardziej złożonych przypadkach, gdzie potrzebujemy dokładnie określić wzorzec do zastąpienia.

Dzięki możliwościom języka Elixir, możemy także zastosować funkcję ```Enum.map/2```, która pozwala na zmapowanie funkcji na każdy element w danym zbiorze. W ten sposób możemy odnaleźć wyrażenia spełniające określone kryteria i dokonać na nich zmiany.

## Zobacz także

Jeśli jesteś zainteresowany/zainteresowana zagłębieniem się w zagadnienia wyszukiwania i zamiany tekstu w języku Elixir, polecamy zapoznać się z poniższymi źródłami:

- [Dokumentacja Elixir](https://elixir-lang.org/docs.html)
- [Oficjalny kurs Elixir na Udemy](https://www.udemy.com/course/the-complete-elixir-and-phoenix-bootcamp-and-tutorial/)
- [Poradniki i artykuły na temat Elixir na Medium](https://medium.com/search?q=Elixir)