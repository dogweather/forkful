---
title:                "PHP: Odczytywanie argumentów linii poleceń"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego warto poznać obsługę argumentów wiersza poleceń w PHP? W skrócie, umożliwia to dostosowywanie działania programu poprzez dostarczanie mu informacji w momencie jego uruchamiania. W ten sposób możesz wpływać na sposób działania swojego programu bez konieczności wprowadzania zmian w samym kodzie.

## Jak to zrobić

Aby zacząć korzystać z argumentów wiersza poleceń w PHP, musimy najpierw zdefiniować, które argumenty będą wymagane, a które opcjonalne. W tym celu wykorzystamy funkcję `getopt()`, która zwraca tablicę zawierającą przetworzone argumenty wiersza poleceń. Poniżej znajduje się przykładowy kod:

```PHP
$options = getopt("h:l", ["help", "log"]);
```

W tym przypadku, `h` i `l` są opcjonalnymi argumentami, a `help` i `log` są wymaganymi. Teraz, jeśli chcemy uruchomić nasz program z opcjonalnym argumentem `h` o wartości `5`, możemy wpisać w terminalu:

```
php program.php -h 5
```

Otrzymamy wtedy w naszej tablicy zwróconej przez `getopt()` wartość `5` pod kluczem `h`.

## Deep Dive

Oprócz podstawowej obsługi argumentów opcjonalnych i wymaganych, funkcja `getopt()` umożliwia również bardziej skomplikowane akcje, takie jak ustawianie domyślnej wartości dla argumentów czy filtrowanie argumentów według określonego wzorca za pomocą wyrażeń regularnych. Warto więc zapoznać się z pełną dokumentacją PHP dotyczącą obsługi argumentów wiersza poleceń, aby w pełni wykorzystać ich możliwości.

## Zobacz również

Zapraszamy do zapoznania się z poniższymi linkami dla dalszego wgłębiania się w temat:

- https://www.php.net/manual/en/function.getopt.php
- https://www.php.net/manual/en/reserved.variables.argv.php
- https://www.php.net/manual/en/function.array-unshift.php