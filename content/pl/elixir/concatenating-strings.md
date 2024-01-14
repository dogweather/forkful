---
title:    "Elixir: Łączenie ciągów tekstowych"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Skonkatynowanie ciągów znaków jest powszechną czynnością w programowaniu. Jest to proces łączenia dwóch lub więcej ciągów znaków w jeden dłuższy ciąg. Może być to przydatne w różnych sytuacjach, na przykład przy tworzeniu wiadomości lub generowaniu dynamicznych zapytań do bazy danych. W Elixirze jest kilka sposobów na skonkatynowanie ciągów znaków, więc warto poznać je wszystkie.

## Jak to zrobić?

### Używając operatora `<>`

Najprostszym sposobem skonkatynowania ciągów w Elixirze jest użycie operatora `<>`. Przykładowy kod wyglądałby tak:

```elixir
"Hello " <> "world" 
```

Wynikiem będzie ciąg znaków "Hello world". Wystarczy połączyć dwa ciągi znaków za pomocą tego operatora, a Elixir automatycznie je skonkatynuje.

### Używając funkcji `String.concat/1`

Elixir również dostarcza funkcję `String.concat/1`, która przyjmuje listę ciągów znaków i zwraca jedną połączoną wartość. Przykład:

```elixir
String.concat(["Hello ", "world"])
```

Wynikiem będzie również ciąg znaków "Hello world".

### Używając interpolacji

Jeśli chcemy połączyć ciągi wewnątrz innego ciągu, możemy skorzystać z interpolacji. Polega to na użyciu `#{}` wewnątrz ciągu i umieszczeniu w nim zmiennych lub wyrażeń, które chcemy połączyć. Przykład:

```elixir
name = "John"
"Hello #{name}!"
```

Wynikiem będzie ciąg "Hello John!". 

## Zagłębienie

Podczas skonkatynowania ciągów znaków należy pamiętać o kilku rzeczach. Po pierwsze, należy uważać na wydajność, zwłaszcza gdy trzeba połączyć wiele ciągów. Używanie operatora `<>` jest szybsze niż użycie funkcji `String.concat/1`, a interpolacja jest szybsza niż oba te sposoby. Po drugie, trzeba uważać na typy danych. Należy pamiętać, że podczas skonkatynowania również może dojść do konwersji typów, co może być niepożądane w niektórych sytuacjach.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o operacjach na ciągach w Elixirze, polecamy zapoznać się z poniższymi linkami:

- [Oficjalna dokumentacja Elixir](https://hexdocs.pm/elixir/String.html)
- [Blog Elixir School](https://elixirschool.com/pl/lessons/basics/strings/)
- [Podstawy języka Elixir - konkatenacja](https://miroslawzelent.pl/kurs-elixir/podstawy/konkatenacja/)