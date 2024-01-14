---
title:    "Fish Shell: Tworzenie pliku tymczasowego"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego tworzyć tymczasowe pliki?

Tworzenie tymczasowych plików jest przydatne, gdy chcemy przetworzyć dane lub wykonać jakieś operacje bez naruszania oryginalnych danych. Może to być szczególnie pomocne podczas programowania w języku Fish Shell. Ale dlaczego warto to robić?

Tworzenie tymczasowych plików może pomóc w zabezpieczeniu danych przed niechcianymi modyfikacjami lub utratą danych. Jest to również przydatne w przypadku konieczności wykonania operacji na dużych zbiorach danych, które mogą wpłynąć na wydajność systemu. Teraz zastanówmy się, jak to zrobić w praktyce.

## Jak to zrobić?

Aby stworzyć tymczasowy plik w języku Fish Shell, możemy wykorzystać wbudowane polecenie `mktemp`. Przyjmuje ono jeden argument w postaci szablonu nazwy pliku, którym będzie ono generowane. Na przykład:

```Fish Shell
mktemp example_XXXXXX.txt
```
W powyższym przykładzie, `XXXXXX` zostanie zastąpione sześcioma losowymi znakami, tworząc unikatową nazwę pliku. Po wykonaniu tego polecenia, zostanie wygenerowany oraz utworzony plik `example_AbCdeF.txt`.

Możemy również określić ścieżkę do miejsca, w którym chcemy utworzyć plik. Na przykład:

```Fish Shell
mktemp -p /home/user/documents/ example_XXXXXX.txt
```
Polecenie to utworzy plik `example_AbCdeF.txt` w podanej lokalizacji.

## Głębszy wgląd

W przypadku gdy potrzebujemy utworzyć tymczasowy plik o określonej zawartości, możemy wykorzystać polecenie `mktemp` w połączeniu z przekierowaniem strumienia wyjścia do nowego pliku, na przykład:

```Fish Shell
echo "Hello World!" > $(mktemp example_XXXXXX.txt)
```
W ten sposób, po wykonaniu polecenia, zostanie wygenerowany i utworzony plik `example_AbCdeF.txt` z zawartością "Hello World!".

Należy pamiętać, że utworzone tymczasowe pliki nie zostaną automatycznie usunięte po zakończeniu pracy. Musimy sami zadbać o ich usuwanie w celu uniknięcia zapełnienia przestrzeni dyskowej. Możemy wykorzystać polecenie `rm` do tego celu.

## Zobacz również

- [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tworzenie tymczasowych plików w języku Bash](https://www.linode.com/docs/guides/create-temporary-files-in-bash/)
- [Tworzenie tymczasowych plików w języku Python](https://docs.python.org/3/library/tempfile.html)