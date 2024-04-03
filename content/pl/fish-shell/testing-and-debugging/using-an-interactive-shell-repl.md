---
date: 2024-01-26 04:14:04.961096-07:00
description: "REPL, czyli Read-Eval-Print Loop (P\u0119tla Czytaj-Wykonaj-Wydrukuj),\
  \ to interaktywne \u015Brodowisko programistyczne, kt\xF3re przyjmuje pojedyncze\
  \ wej\u015Bcia\u2026"
lastmod: '2024-03-13T22:44:35.842997-06:00'
model: gpt-4-0125-preview
summary: "REPL, czyli Read-Eval-Print Loop (P\u0119tla Czytaj-Wykonaj-Wydrukuj), to\
  \ interaktywne \u015Brodowisko programistyczne, kt\xF3re przyjmuje pojedyncze wej\u015B\
  cia u\u017Cytkownika, wykonuje je i zwraca wynik."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

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
