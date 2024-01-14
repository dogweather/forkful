---
title:    "PHP: Sprawdzanie czy istnieje katalog"
keywords: ["PHP"]
---

{{< edit_this_page >}}

### Dlaczego

Jeśli zajmujesz się programowaniem w PHP, istnieje duże prawdopodobieństwo, że prędzej czy później będziesz musiał sprawdzić istnienie określonego katalogu. Może to być wymagane, aby upewnić się, że odpowiednie pliki są dostępne lub aby uniknąć błędów w kodzie. W tym blogu omówimy, dlaczego i jak należy sprawdzać, czy katalog istnieje w PHP.

### Jak to zrobić

Najprostszym sposobem sprawdzenia, czy dany katalog istnieje w PHP, jest użycie funkcji `file_exists()`. Przyjmuje ona ścieżkę do katalogu jako argument i zwraca wartość `true` lub `false`, w zależności od tego, czy katalog istnieje czy nie. Poniżej znajduje się przykładowy kod:

```PHP
if (file_exists('sciezka/do/katalogu')) {
    echo 'Katalog istnieje!';
} else {
    echo 'Katalog nie istnieje!';
}
```

Jeśli katalog istnieje, zostanie wyświetlone pierwsze zdanie, a jeśli nie istnieje - drugie. Możesz również użyć tej funkcji w warunkach lub pętlach, aby wykonać odpowiednią logikę w zależności od istnienia katalogu. Istnieje także inna funkcja, `is_dir()`, która sprawdza, czy dany element jest katalogiem, a nie innym typem pliku.

```PHP
if (is_dir('sciezka/do/katalogu')) {
    // wykonaj jakieś działania związane z tym katalogiem
}
```

### Głębsze zagłębianie się

Wraz z rozwojem PHP, powstało wiele innych funkcji związanych ze sprawdzaniem istnienia katalogów, takich jak `glob()`, `scandir()` czy `opendir()`. Każda z nich ma swoje zastosowanie i może być przydatna w różnych sytuacjach. Jeśli chcesz się bardziej zagłębić w temat sprawdzania katalogów w PHP, możesz przeczytać dokumentację na oficjalnej stronie języka.

### Zobacz też

- [Dokumentacja PHP: file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- [Dokumentacja PHP: is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [Dokumentacja PHP: glob()](https://www.php.net/manual/en/function.glob.php)