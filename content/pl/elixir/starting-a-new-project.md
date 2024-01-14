---
title:    "Elixir: Rozpoczynając nowy projekt"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiasz się, dlaczego powinieneś zacząć nowy projekt w Elixir? Jest wiele powodów, dlaczego ta język programowania jest warty Twojego czasu. Elixir jest wydajny, skalowalny i oparty na niesamowitym silniku działającym na Erlangu. Ponadto, jego składnia jest przyjazna dla programistów i bardzo dobrze radzi sobie z równoległym wykonywaniem zadań.

## Jak zacząć

Zacznij od zainstalowania Elixir na swoim komputerze. Następnie możesz wybrać jeden z wielu dostępnych edytorów tekstowych, takich jak Emacs, Vim lub Visual Studio Code. Jeśli używasz Maca, możesz również rozważyć instalację aplikacji "Elixir for Mac". W następnym kroku zapoznaj się z dokumentacją języka Elixir oraz jego podstawowymi konceptami, takimi jak funkcje i wzorce dopasowania.

```Elixir
defmodule Hello do
  def greeting(name) do
    "Hello, #{name}!"
  end
end

IO.puts Hello.greeting("John")
```

Output: Hello, John!

Teraz pora nauczyć się korzystać z narzędzi w Elixir, takich jak mieszaniec (ang. mix). Jest to narzędzie do tworzenia, zarządzania i uruchamiania projektów w Elixir. Umożliwia ono także instalację zależności projektu oraz uruchomienie testów jednostkowych.

## Głębszy wgląd

Aby lepiej zrozumieć Elixir, warto poznać jego podstawowe struktury danych, takie jak listy, mapy czy tupple. Ponadto, warto zapoznać się z mechanizmem kompilacji i działania maszyny wirtualnej Erlanga. W projekcie w Elixir można także wykorzystać wzorce projektowe, takie jak supervisor, który pomoże w kontroli wątków i naprawie ewentualnych błędów.

## Zobacz także

Jeśli jesteś zainteresowany lub zaciekawiony Elixirem, zapraszamy do zapoznania się z poniższymi linkami:

- [Dokumentacja języka Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Phoenix Framework - najpopularniejszy framework webowy w Elixirze](https://www.phoenixframework.org/)
- [Elixir School - darmowe interaktywne kursy Elixir](https://elixirschool.com/pl/)
- [Reddit Elixir - społeczność Elixir na Reddit](https://www.reddit.com/r/elixir/)