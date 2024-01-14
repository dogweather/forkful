---
title:    "Fish Shell: Usuwanie znaków pasujących do wzoru"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w naszych skryptach powłoki potrzebujemy usunąć pewne znaki, które pasują do określonego wzorca. Jest to szczególnie przydatne w przypadku operacji na tekście. W tym artykule przedstawimy, jak to zrobić w powłoce Fish Shell.

## Jak to zrobić

W Fish Shell istnieje wbudowana funkcja `string delete`, która pozwala na usunięcie znaków pasujących do podanego wzorca. Przykładowe wywołanie wygląda następująco:

```Fish Shell
set my_string "Hello World"
string delete -ra "l" $my_string
```

W tym kodzie wywołujemy `string delete`, z opcją `-ra` oznaczającą "wszystkie znaki", a następnie podajemy wzorzec, który chcemy usunąć, w tym przypadku jest to litera "l". Podajemy również zmienną zawierającą nasz tekst, czyli "Hello World". Wywołanie to spowoduje usunięcie wszystkich wystąpień litery "l" z tekstu i wyświetlenie wyniku, który w tym przypadku będzie brzmiał "Heo Word".

Jeśli chcemy usunąć znaki tylko z określonej pozycji w tekście, możemy wykorzystać opcję `-i`, np:

```Fish Shell
set my_string "Hello World"
string delete -ia 0..3 $my_string
```

Tym razem usuniemy znaki z pozycji 0 do 3, czyli "Hell" zostanie zamienione na pusty ciąg znaków, a wynikiem będzie "o World".

## Deep Dive

Funkcja `string delete` w Fish Shell jest bardzo użyteczna, ponieważ umożliwia nam dokładne kontrolowanie, które znaki chcemy usunąć. Pamiętajmy jednak, że gdy podajemy opcję `-ra`, wszystkie wystąpienia wzorca zostaną usunięte. Jeśli chcemy usunąć tylko jedno wystąpienie, musimy określić konkretną pozycję lub użyć opcji `-i` i podać zakres pozycji.

## Zobacz też

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/
- Przewodnik po Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Opis funkcji `string delete`: https://fishshell.com/docs/current/cmds/string.html#string-delete