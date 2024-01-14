---
title:    "Fish Shell: Sprawdzanie czy istnieje katalog"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy dany katalog istnieje, jest ważną częścią tworzenia skryptów w języku Fish Shell. Pozwala to na sprawdzenie, czy konkretny katalog jest dostępny i przetwarzanie odpowiedniego kodu w zależności od wyniku.

## Jak to zrobić

Programowanie w języku Fish Shell jest bardzo intuicyjne i prostsze niż w innych powłokach systemowych. Aby sprawdzić istnienie katalogu, wystarczy użyć polecenia `test` wraz z flagą `-d`, a następnie podać ścieżkę do katalogu.

```Fish Shell
if test -d /sciezka/do/katalogu
    echo "Katalog istnieje"
end
```

Jeśli katalog istnieje, to zostanie wyświetlony komunikat "Katalog istnieje". W przeciwnym razie, nic się nie wyświetli.

## Zagłębienie się w temat

Aby lepiej zrozumieć to, jak działa sprawdzanie istnienia katalogu w języku Fish Shell, należy wiedzieć, że polecenie `test` to w rzeczywistości skrót dla polecenia `[[` (podwójny nawias kwadratowy). Ten ostatni jest bardziej rozbudowanym poleceniem i umożliwia dokładniejsze sprawdzanie warunków, w tym czy dany katalog istnieje.

```Fish Shell
if [[ -d /sciezka/do/katalogu ]]
    echo "Katalog istnieje"
end
```

Ponadto, w języku Fish Shell można używać wyrażeń regularnych, aby sprawdzić czy dana ścieżka pasuje do konkretnego wzorca. Przykładowo, aby sprawdzić czy w ścieżce znajduje się słowo "test", można użyć następującego kodu:

```Fish Shell
if [[ "/sciezka/do/katalogu/test" =~ "test" ]]
    echo "Ścieżka zawiera słowo test"
end
```

## Zobacz też

- [Podstawy programowania w języku Fish Shell](https://example.com)
- [Dokumentacja języka Fish Shell](https://example.com)