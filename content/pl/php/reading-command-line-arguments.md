---
title:                "PHP: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą PHP i pracujesz z plikami wiersza poleceń, prawdopodobnie zastanawiasz się, dlaczego warto czytać argumenty wiersza poleceń. W tym artykule dowiecie się o niezbędnym narzędziu i jak korzystać z niego w swoich projektach.

## Jak to zrobić

Aby odczytać argumenty wiersza poleceń w PHP, wystarczy skorzystać z wbudowanej funkcji `getopt()`. Poniżej znajduje się przykładowy kod, wykorzystujący tę funkcję:

```PHP
$options = getopt("f:h", ["file:", "help"]);
```

W powyższym przykładzie, pierwszy parametr funkcji `getopt()` oznacza listę akceptowanych opcji, gdzie każda litera to jedna opcja, a dwukropki oznaczają, że akceptowane są także opcje z parametrem. Drugi parametr to tablica z opcjami, które akceptujemy w formacie dłuższym, gdzie pierwszy element to nazwa opcji, a drugi to jej skrót.

Następnie należy wykorzystać pętlę `foreach` do odczytania wartości poszczególnych argumentów:

```PHP
foreach ($options as $key => $value) {
    switch ($key) {
        case 'f':
        case 'file':
            echo "Odczytano argument 'file' o wartości: " . $value;
            break;
        case 'h':
        case 'help':
            echo "Wyświetlam pomoc dotyczącą aplikacji.";
            break;
    }
}
```

Powyższy kod odczyta dwa argumenty: `-f` lub `--file`, a także `-h` lub `--help`. W zależności od wprowadzonych opcji, zostaną wykonane odpowiednie akcje, np. wyświetlenie wartości podanego pliku lub wyświetlenie pomocy dla użytkownika.

## Deep Dive

Funkcja `getopt()` pozwala nam na wiele zaawansowanych operacji związanych z odczytywaniem argumentów wiersza poleceń. Dzięki niej, możemy kontrolować m.in. kolejność wprowadzania argumentów, obsługiwać parametry opcji czy też walidować wprowadzone wartości.

Istnieje również wiele innych funkcji i bibliotek, które mogą ułatwić pracę z argumentami wiersza poleceń w PHP. Warto poznać je wszystkie, aby w pełni wykorzystać możliwości, jakie daje nam to narzędzie.

## Zobacz również

- [Dokumentacja PHP - getopt()](https://www.php.net/manual/en/function.getopt.php)
- [Kurs z wiersza poleceń w PHP](https://codecourse.com/watch/command-line/php-command-line-basics)
- [PHP CLI Parser - biblioteka do parsowania argumentów wiersza poleceń](https://github.com/dericofilho/php-cli-parser)