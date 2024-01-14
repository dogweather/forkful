---
title:    "Elixir: Sprawdzanie, czy istnieje katalog."
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie istnienia katalogu jest ważnym elementem w programowaniu w Elixir. Pozwala na upewnienie się, czy dany katalog istnieje przed wykonaniem operacji na nim. Jest to szczególnie przydatne w przypadku tworzenia plików lub przetwarzania danych w istniejącym katalogu.

## Jak to zrobić

Można to zrobić przy pomocy funkcji `File.stat/1`, która przyjmuje ścieżkę do katalogu jako argument i zwraca informacje o pliku. Jeśli katalog istnieje, funkcja zwróci `%File.Stat{}` z odpowiednimi wartościami, w przeciwnym razie zostanie zgłoszony błąd.

```Elixir
File.stat("moj_katalog/") 
# => %File.Stat{dev: 234881026, ino: 562949953594879, mode: 16877, nlink: 3, uid: 501, gid: 20, rdev: 0, size: 96, blksize: 4096, blocks: 0, atime: {2019, 1, 29, 15, 19, 13}, mtime: {2019, 1, 29, 15, 19, 13}, ctime: {2019, 1, 29, 15, 19, 13}}
```

W powyższym przykładzie, katalog "moj_katalog/" istnieje, więc funkcja zwraca informacje o nim.

Jeśli chcemy sprawdzić tylko istnienie katalogu, możemy skorzystać z metody `File.dir?/1`, która zwraca wartość logiczną (true/false).

```Elixir
File.dir?("moj_katalog/") 
# => true
```

W przypadku, gdy katalog nie istnieje, funkcja zwróci `false`.

```Elixir
File.dir?("nieistniejacy_katalog/") 
# => false
```

## Głębszy przegląd

Sprawdzanie istnienia katalogu może być również przydatne w przypadku tworzenia aplikacji webowych. Możemy wykorzystać tę funkcję do sprawdzenia, czy istnieje katalog dla konkretnego użytkownika, a następnie wykorzystać go do zapisu plików lub przetwarzania danych. Możemy również dodać warunki, które będą wyświetlać komunikat o błędzie lub prosić użytkownika o utworzenie katalogu w razie jego braku.

## Zobacz też

- Dokumentacja Elixir: [https://hexdocs.pm/elixir/File.html#stat/1](https://hexdocs.pm/elixir/File.html#stat/1)
- Przykładowe wykorzystanie funkcji `File.stat/1` w aplikacji webowej: [https://github.com/elixir-lang/plug/blob/master/lib/plug/multipart.ex#L72](https://github.com/elixir-lang/plug/blob/master/lib/plug/multipart.ex#L72)
- Poradnik o obsłudze plików i katalogów w Elixir: [https://elixir-lang.org/getting-started/file-operations.htm/](https://elixir-lang.org/getting-started/file-operations.html)