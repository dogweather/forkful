---
title:                "Rejestrowanie zdarzeń"
date:                  2024-01-26T01:03:37.290399-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Rejestrowanie (logging) to w zasadzie nasz sposób zapisywania tego, co się dzieje w naszych programach. To jakby mieć małą czarną skrzynkę; gdy coś pójdzie nie tak (i uwierz mi, na pewno tak się stanie), logi są nieocenione do rozpracowania co się wydarzyło, diagnostyki problemów i optymalizacji wydajności.

## Jak to zrobić:
W Gleam, zazwyczaj dodajesz bibliotekę do rejestrowania — nie ma dedykowanego mechanizmu rejestrowania "od ręki". Przypuśćmy, że używamy hipotetycznego pakietu `gleam_logger`. Oto jak można go zintegrować:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("Aplikacja się uruchamia!")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Obliczenie zakończone sukcesem", value)
    Error(err) -> 
      gleam_logger.error("Obliczenie nieudane", err)
  }
}
```

Oczekiwany wynik w logach powinien wyglądać mniej więcej tak:

```
INFO: Aplikacja się uruchamia!
DEBUG: Obliczenie zakończone sukcesem 42
ERROR: Obliczenie nieudane Powód: Dzielenie przez zero
```

## Pogłębiona analiza
Sztuka rejestrowania istnieje od wczesnych dni programowania. Operatorzy systemów rzeczywiście otrzymywali logi z komputera — dbali o to, aby wszystko działało płynnie. Po latach, rejestrowanie stało się cyfrowe, stając się kluczową częścią rozwoju oprogramowania.

Chociaż Gleam, będąc stosunkowo młodym językiem, który jest przeznaczony dla ekosystemu Erlanga, nie ma wbudowanego systemu rejestrowania, można wykorzystać dojrzałe możliwości rejestrowania Erlanga lub inne biblioteki dostarczone przez społeczność. Każda z nich ma różne funkcje i kompromisy: niektóre mogą oferować strukturalne rejestrowanie, inne są bardziej do prostego tekstowego wyjścia.

Teraz pytanie o implementację mechanizmu rejestrowania: Czy to proste? Na pierwszy rzut oka, tak. Ale gdy zgłębisz temat, zaczniesz mierzyć się z obsługą współbieżności, wąskimi gardłami I/O, rotacją logów, standaryzacją formatów (pomyśl o JSON dla strukturalnego rejestrowania), filtrowaniem poziomów logowania, a możliwe że nawet ze śledzeniem rozproszonym. Co więcej, w paradygmacie funkcyjnym, zazwyczaj chcesz, aby efekty uboczne (takie jak rejestrowanie) były obsługiwane w przewidywalny i kontrolowany sposób.

## Zobacz także
Oto gdzie możesz znaleźć więcej na temat podstaw i technik rejestrowania w Gleam i otaczającym go ekosystemie:
- [Dokumentacja :logger Erlanga](http://erlang.org/doc/apps/kernel/logger_chapter.html): Ponieważ Gleam kompiluje do Erlanga, jest to bezpośrednio stosowalne.
- [Dokumentacja standardowej biblioteki Gleam](https://hexdocs.pm/gleam_stdlib/): Aby uzyskać aktualizacje na temat wszelkich narzędzi do rejestrowania, które mogą zostać dodane.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): Starannie wyselekcjonowana lista zasobów, która może zawierać biblioteki do rejestrowania, kiedy staną się dostępne.