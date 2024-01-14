---
title:    "Fish Shell: Odczytywanie pliku tekstowego"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś lub musiałeś przeczytać plik tekstowy przy użyciu powłoki Fish Shell? Ten prosty przewodnik pokaże Ci, jak to zrobić oraz dlaczego jest to przydatna umiejętność. Czytanie plików tekstowych jest niezwykle ważne w programowaniu, ponieważ pozwala nam na przeglądanie i przetwarzanie danych w łatwy sposób.

## Jak

Zacznijmy od załadowania pliku, który chcemy przeczytać. W tym przykładzie użyjemy polecenia `cat` do wyświetlenia zawartości pliku "tekst.txt":

```Fish Shell
cat tekst.txt
```

To polecenie wyświetli całą zawartość pliku tekstowego w oknie terminala. Jeśli chcesz wyświetlić tylko kilka pierwszych linijek, możesz użyć polecenia `head`:

```Fish Shell
head -n 5 tekst.txt
```

W ten sposób wyświetlimy tylko pierwsze 5 linijek pliku. Podobnie, jeśli chcesz wyświetlić tylko kilka ostatnich linijek, możesz użyć polecenia `tail`:

```Fish Shell
tail -n 3 tekst.txt
```

Ten przykład wyświetli ostatnie 3 linijki pliku. Inną przydatną funkcją jest wyszukiwanie słów lub fraz w pliku tekstowym. Możemy to zrobić za pomocą polecenia `grep`:

```Fish Shell
grep "słowo" tekst.txt
```

Ten przykład wyświetli wszystkie linijki, w których występuje słowo "słowo". Jako że jest to tylko wprowadzenie do czytania plików tekstowych w Fish Shell, zapraszam do eksperymentowania z innymi poleceniami i opcjami.

## Deep Dive

Powyższe przykłady pokazały podstawowe sposoby czytania plików tekstowych w Fish Shell. Możesz jednak również użyć bardziej zaawansowanych technik, takich jak przekierowanie danych. Na przykład, aby wyświetlić zawartość pliku tekstowego w innym pliku, możesz użyć polecenia `tee`:

```Fish Shell
cat tekst.txt | tee nowy_plik.txt
```

Ten przykład wyświetli zawartość pliku "tekst.txt" i jednocześnie zapisze ją do nowego pliku "nowy_plik.txt".

Fish Shell oferuje również wiele opcji i funkcji, aby pomóc w przetwarzaniu dużych plików tekstowych. Na przykład, jeśli chcesz wyświetlić tylko określoną część pliku, możesz użyć polecenia `less`:

```Fish Shell
less tekst.txt
```

Polecenie to pozwala na przewijanie i przeszukiwanie dużych plików. Więcej informacji na temat tego polecenia można znaleźć w dokumentacji Fish Shell.

## Zobacz także

Teraz, gdy już wiesz jak czytać pliki tekstowe w Fish Shell, możesz wykorzystać tę umiejętność w swoim kodzie. Poniżej znajdują się przydatne linki, które mogą Ci pomóc w dalszym zgłębianiu tej tematyki:

- [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial o czytaniu plików tekstowych w Fish Shell](https://likegeeks.com/fish-shell-scripts/)
- [Poradnik poświęcony czytaniu i przetwarzaniu danych w Fish Shell](http://wooll.blog/2015/08/reading-and-processing-files-in-fish-shell/)
- [Przydatne polecenia do przetwarzania danych w Fish Shell](https://fishshell.unixwitch.me/fish/include-fish/page.html)

Dziękuję za przeczytanie! Mam nadzieję, że ten artykuł był dla Ciebie przydatny!