---
title:                "Modyfikowanie plików za pomocą jednolinijkowców CLI"
date:                  2024-01-26T22:25:09.090322-07:00
model:                 gpt-4-0125-preview
simple_title:         "Modyfikowanie plików za pomocą jednolinijkowców CLI"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Modyfikowanie plików przy użyciu jednolinijkowych poleceń CLI (Command Line Interface) w Ruby polega na szybkim i często prostym manipulowaniu tekstem bezpośrednio z terminala, wykorzystując opcje linii poleceń Ruby. Ta technika jest nieoceniona, gdy potrzebujesz przeprowadzić zmiany wsadowe w plikach, filtrować zawartość lub automatyzować zadania edycyjne bez otwierania edytora. Chodzi o wykorzystanie możliwości przetwarzania tekstu przez Ruby w sposób efektywny dla skryptowalnych edycji.

## Jak to zrobić:
Załóżmy, że masz plik o nazwie `example.txt` z kilkoma liniami tekstu i chcesz odwrócić kolejność linii. Z Ruby możesz to zrobić jednym poleceniem:

```ruby
ruby -e 'puts File.readlines("example.txt").reverse' 
```

Lub, jeśli chcesz zastąpić wszystkie wystąpienia "foo" przez "bar" w `data.txt`, możesz zrobić:

```ruby
ruby -i.bak -pe 'gsub(/foo/, "bar")' data.txt
```

To polecenie tworzy również kopię zapasową (`data.txt.bak`) oryginalnego pliku, pokazując, jak Ruby dba o bezpieczeństwo danych. Przykładowy wynik nie jest bezpośrednio widoczny, ponieważ te polecenia zmieniają zawartość pliku, ale możesz użyć `cat data.txt`, aby zobaczyć zmiany.

## Głębsze spojrzenie
Flaga `-e` mówi Ruby, aby wykonało dany skrypt, podczas gdy `-i` umożliwia edycję na miejscu z opcjonalnym rozszerzeniem, aby utworzyć plik kopii zapasowej. Flaga `-p` przetwarza wejście i drukuje każdą linię po zastosowaniu skryptu, podobnie do sed w Unix/Linux.

Historycznie rzecz biorąc, edycja na miejscu i przetwarzanie linii poleceń były domenami zdominowanymi przez sed, awk i perl. Ruby jednak ładnie włącza te funkcjonalności, umożliwiając bardziej złożone manipulacje dzięki swojej bogatej składni i wbudowanym bibliotekom.

Alternatywy dla modyfikacji plików obejmują sed i awk do prostszych zadań lub użycie pełnych skryptów Ruby do bardziej złożonego przetwarzania. Minusem użycia Ruby do jednolinijkowców może być wydajność dla bardzo dużych plików lub złożonych operacji, gdzie narzędzia zaprojektowane specjalnie do przetwarzania tekstu mogą działać szybciej.

Pod względem implementacji, gdy Ruby przetwarza pliki w linii, efektywnie tworzy tymczasowe wyjście podczas odczytywania pliku, a następnie zastępuje oryginalny plik tym wyjściem. Ten szczegół podkreśla znaczenie opcji tworzenia kopii zapasowych lub ostrożnego testowania z użyciem flagi `-i`, aby uniknąć utraty danych.

## Zobacz także
- Oficjalna dokumentacja Ruby dotycząca opcji linii poleceń: [https://www.ruby-lang.org/en/documentation/quickstart/3/](https://www.ruby-lang.org/en/documentation/quickstart/3/)
- Obszerne porównanie przetwarzania tekstu w Ruby vs. sed i awk: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- Aby zgłębić temat obsługi plików i IO przez Ruby: [https://ruby-doc.org/core-2.7.0/IO.html](https://ruby-doc.org/core-2.7.0/IO.html)
