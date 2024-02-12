---
title:                "Korzystanie z interaktywnego shella (REPL)"
date:                  2024-01-26T04:14:04.961096-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
REPL, czyli Read-Eval-Print Loop (Pętla Czytaj-Wykonaj-Wydrukuj), to interaktywne środowisko programistyczne, które przyjmuje pojedyncze wejścia użytkownika, wykonuje je i zwraca wynik. Programiści używają go do uzyskiwania natychmiastowego informacji zwrotnej, debugowania oraz dynamicznego eksperymentowania z koncepcjami kodowania bez konieczności kompilowania i uruchamiania pełnego programu.

## Jak to zrobić:
W Fish, interaktywna powłoka jest domyślnym trybem, gdy ją uruchamiasz. Oto jak to wygląda w akcji:

```Fish Shell
> set color blue
> echo "Niebo jest $color"
Niebo jest niebieskie
```

Możesz również uruchamiać wbudowane funkcje i bawić się substytucjami poleceń:

```Fish Shell
> function cheer
      echo "Naprzód Fish $argv!"
  end
> cheer Programiści
Naprzód Fish Programiści!
```

Nie tylko definiowanie funkcji, możesz wykonywać fragmenty kodu na bieżąco i od razu widzieć wynik:

```Fish Shell
> math "40 / 2"
20
```

## Pogłębiona analiza
Koncepcja REPL sięga języka programowania Lisp w latach 60. Ta forma interaktywnego programowania ustawiła wzorzec dla środowisk takich jak `ipython` w Pythonie i `irb` w Ruby. Fish kontynuuje ten trend, koncentrując się na przyjazności dla użytkownika i użyciu interaktywnym.

Fish różni się od innych powłok takich jak Bash tym, że od samego początku jest zaprojektowany z myślą o interaktywności. Oferuje kolorowanie składni, autosugestie i uzupełnianie tabulacyjne, co czyni go potężnym narzędziem do użytku w stylu pracy REPL. Co więcej, twoje polecenia są zapamiętywane i przeszukiwane, co ułatwia powtarzanie testów.

Alternatywy dla REPL Fisha mogą obejmować `bash` lub `zsh` w połączeniu z rozszerzeniami takimi jak `bash-completion` lub `oh-my-zsh`, ale Fish ma tendencję do oferowania bogatszego doświadczenia „z pudełka”.

## Zobacz również:
- Dokumentacja Fish: https://fishshell.com/docs/current/index.html
- Interesujące porównanie Fish i innych powłok: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- Pogłębione zagłębienie się w REPL: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Programowanie interaktywne w Lispie, historyczne spojrzenie: http://www.paulgraham.com/ilisp.html
