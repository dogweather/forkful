---
title:                "Pobieranie strony internetowej"
html_title:           "Fish Shell: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli potrzebujesz szybkiego sposobu na pobieranie stron internetowych za pomocą wiersza poleceń, to Fish Shell jest dla Ciebie. Jest to potężne narzędzie, które umożliwia łatwe pobieranie zawartości stron internetowych bez konieczności korzystania z przeglądarki.

## Jak to zrobić

### 1. Instalacja Fish Shell

Pierwszym krokiem jest zainstalowanie Fish Shell na swoim komputerze. Możesz to zrobić poprzez menedżer pakietów lub pobranie pliku binarnego ze strony https://fishshell.com/. Po zainstalowaniu, upewnij się, że Fish Shell jest wybranym domyślnym wierszem poleceń.

### 2. Używanie poleceń w Fish Shell

Fish Shell ma wiele wbudowanych poleceń, które ułatwiają pobieranie stron internetowych. Możesz użyć komendy ```curl```, aby pobrać zawartość strony i zapisać ją w pliku tekstowym, na przykład:

```
curl https://www.example.com > example.txt
```

Możesz również użyć komendy ```wget```, która oferuje więcej opcji, takich jak pobieranie całych witryn z podłączonymi plikami, na przykład:

```
wget --mirror -p --convert-links https://www.example.com
```

### 3. Przetwarzanie pobranej zawartości

Fish Shell pozwala również na łatwe przetwarzanie pobranej zawartości. Możesz użyć komendy ```grep```, aby znaleźć konkretne wyrażenia w pobranej stronie, na przykład:

```
grep "example" example.txt
```

Możesz również przetworzyć stronę za pomocą poleceń awk, sed lub perl.

## Głębszy zanurzenie

Fish Shell ma wiele innych przydatnych funkcji związanych z pobieraniem stron internetowych. Możesz na przykład pobierać tylko nagłówki stron przy użyciu komendy ```head```, sprawdzać status strony przy użyciu komendy ```curl```, lub pobierać pliki zabezpieczające (np. certyfikaty SSL) przy użyciu komendy ```openssl```. Możliwości są nieograniczone, a wszystkie te komendy są łatwe w użyciu w Fish Shell.

## Zobacz również

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [Porównanie polesień w Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-cmd-compare)
- [Podstawowe komendy w Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-cmds)