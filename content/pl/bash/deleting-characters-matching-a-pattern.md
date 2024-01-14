---
title:                "Bash: Usuwanie znaków odpowiadających wzoru"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem w programowaniu musimy usuwać pewne znaki z tekstu zgodnie z określonym wzorcem. To może się przydać, jeśli chcemy przetworzyć duże ilości danych lub oczyszczać je z niechcianych elementów. W tym artykule dowiesz się, jak skutecznie usuwać znaki dopasowane do wzorca w języku Bash.

## Jak to zrobić

Usunięcie znaków w języku Bash jest stosunkowo proste i wymaga wykorzystania polecenia `sed`. Przykładowy kod wyglądałby następująco:

```Bash
sed 's/pattern//g' filename
```

W powyższym przykładzie `sed` przeszukuje plik podanym w `filename` i usuwa wszystkie wystąpienia dopasowujące się do podanego wzorca. Aby wyświetlić wynik na ekranie, możemy dodać opcję `-i`.

```Bash
sed -i 's/pattern//g' filename
```

Możemy także określić inny wzorzec zamiast pojedynczego znaku w miejscu `pattern`. Dzięki temu możemy precyzyjniej dostosować usuwanie do naszych potrzeb. Przykładowo, jeśli chcielibyśmy usunąć wszystkie wystąpienia słowa "hello" z tekstu, moglibyśmy użyć następującego polecenia:

```Bash
sed 's/hello//g' filename
```

Usunięte zostaną wszystkie wystąpienia słowa "hello" w podanym pliku. Możemy także zastosować `sed` do wielu plików jednocześnie, używając opcji `-i` oraz podając listę plików oddzielonych spacją.

## Dogłębna analiza

W `sed` można także użyć wyrażeń regularnych, co pozwala na bardziej zaawansowane usuwanie znaków dopasowanych do wzorca. Na przykład, za pomocą wyrażenia regularnego `^pattern` usuwamy wszystkie znaki dopasowane do wzorca, ale tylko na początku linii. Natomiast `$pattern` usuwa dopasowania tylko z końca linii.

Możliwe jest także zastosowanie flagi `IGNORECASE`, która pozwala na usuwanie znaków niezależnie od wielkości liter. Przykładowo, `sed` z flagą `IGNORECASE` usuwałoby wszystkie wystąpienia słowa "hello", niezależnie czy jest napisane z małej czy dużej litery.

Pamiętaj jednak, że `sed` nie zmienia oryginalnego pliku, chyba że wykorzystasz opcję `-i`. W przypadku, gdy chcemy zachować zmiany w oryginalnym pliku, warto wykorzystać polecenie `cp` do stworzenia kopii, a następnie aplikować `sed` na kopii.

## Zobacz także

- [Dokumentacja `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Przykłady użycia `sed`](https://www.linuxtechi.com/sed-command-examples-linux/)
- [Wyrażenia regularne w `sed`](https://www.digitalocean.com/community/tutorials/an-introduction-to-regular-expressions)