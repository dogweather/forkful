---
title:                "Fish Shell: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli często pracujesz z tekstem w programowaniu, to z pewnością spotkałeś się z potrzebą znajdowania i zamiany określonych fragmentów. Możliwe, że miało to miejsce nawet wkrótce po tym, jak nauczyłeś się podstaw programowania. Bez względu na to, czy jesteś początkującym czy doświadczonym programistą, pewnie zgodzisz się z nami, że znajdowanie i zamiana tekstu jest nieodłączną częścią wielu projektów. W tym wpisie dowiesz się, w jaki sposób wykorzystać polecenie ```sed``` wraz z narzędziem Fish Shell, aby wygodnie przeprowadzać operacje wyszukiwania i zamiany tekstu.

## Jak to zrobić

Fish Shell posiada wbudowane wsparcie dla poleceń wywołanych z poziomu bazy danych. Jednym z tych poleceń jest ```sed``` - narzędzie do przeprowadzania operacji na tekście. Aby go wykorzystać, wystarczy wpisać polecenie w terminalu i przekazać mu odpowiednie parametry.

Poniżej przedstawiamy przykładowe polecenie, które znajduje wszystkie wystąpienia słowa "kot" w pliku ```tekst.txt``` i zamienia je na "pies":

```
sed -i 's/kot/pies/g' tekst.txt
```

W powyższym przykładzie użycie opcji ```-i``` oznacza, że zmiany zostaną dokonane bezpośrednio w pliku, zamiast wypisania wyniku na ekranie.

## Deep Dive

Polecenie ```sed``` oferuje wiele możliwości i opcji, dzięki którym możemy dokładnie określić, jakie wyrażenia chcemy wyszukać i zamienić. Możemy między innymi wykorzystać flagi, aby określić zakres linii, na którym ma zostać przeprowadzona zmiana, lub zastosować wyrażenia regularne, aby bardziej dokładnie dopasować szukane słowa.

Żeby przetestować różne opcje polecenia ```sed```, polecamy skorzystać z dokumentacji Fish Shell oraz przeglądać przykłady na forach lub blogach programistycznych. W ten sposób możesz przystosować polecenie do swoich konkretnych potrzeb i dokładnie rozumieć, jak działa.

## Zobacz też

- [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/cmds/sed.html)
- [Przykłady użycia polecenia sed w Fish Shell](https://www.baeldung.com/linux/sed-replace-string)
- [Przewodnik po wyrażeniach regularnych w poleceniu sed](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux)

Dziękujemy, że przeczytałeś ten wpis. Mamy nadzieję, że teraz czujesz się pewniej w używaniu polecenia ```sed``` w swoich projektach. W razie potrzeby zawsze możesz wrócić do tego artykułu lub zapoznać się z innymi narzędziami tekstowymi oferowanymi przez Fish Shell.