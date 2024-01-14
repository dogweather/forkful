---
title:                "Gleam: Rozpoczęcie nowego projektu"
programming_language: "Gleam"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiasz się, dlaczego warto zacząć nowy projekt w Gleam? Oto krótkie wyjaśnienie: Gleam to język programowania funkcyjnego, który ma prostą składnię i silnie typowany system, co pozwala na pisanie bezpiecznego i wydajnego kodu. Jego modułowość i wydajność sprawiają, że jest idealnym wyborem dla projektów o dużej skali.

## Jak zacząć

Jeśli chcesz rozpocząć nowy projekt w Gleam, pierwszą rzeczą, którą musisz zrobić, to zainstalować język na swoim komputerze. Możesz to zrobić, korzystając z narzędzia Managera pakietów dla Gleam - Rebar3. Następnie, utwórz nowy katalog projektu i uruchom polecenie `gleam new <nazwa projektu>` aby utworzyć szablon projektu. Możesz także skorzystać z gotowego szablonu dostępnego w repozytorium Gleam, aby szybko rozpocząć pracę.

Po utworzeniu projektu, możesz już zacząć pisać kod w języku Gleam. Przykładowe pliki o rozszerzeniu `.gleam` można znaleźć w katalogu `src` w utworzonym przez Ciebie projekcie. W poniższym przykładzie, utworzymy prosty `hello_world.gleam` i wydrukujemy na ekranie wiadomość "Witaj świecie" za pomocą funkcji `io.print`:

```Gleam
import gleam/io

pub fn main() {
    io.print("Witaj świecie")
}
```

## Deep Dive

Po zainstalowaniu Gleam i utworzeniu szablonu projektu, możesz rozpocząć bardziej dogłębną naukę języka. Na stronie internetowej Gleam znajdziesz dokładną dokumentację, przykładowe projekty oraz linki do społeczności na Slacku i Discourse, gdzie możesz uzyskać pomoc i wymieniać się doświadczeniami z innymi programistami.

Istnieją również biblioteki dostępne dla języka Gleam, które można wykorzystać w swoim projekcie. W repozytorium Gleam znajdziesz listę dostępnych bibliotek oraz informacje o tym, jak je zainstalować i używać.

## Zobacz również

- Strona główna języka Gleam: https://gleam.run/
- Repozytorium Gleam: https://github.com/gleam-lang/gleam
- Dokumentacja języka Gleam: https://gleam.run/documentation/
- Społeczność na Slacku: https://gleam-lang.slack.com/
- Forum na Discourse: https://elixirforum.com/c/gleam/24
- Lista dostępnych bibliotek: https://github.com/gleam-lang/gleam/wiki/Libraries