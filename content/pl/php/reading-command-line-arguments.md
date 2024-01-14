---
title:    "PHP: Odczytywanie argumentów wiersza poleceń"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

 W dzisiejszych czasach, programowanie jest nieodłącznym elementem wielu dziedzin życia. Aby być skutecznym programistą, ważne jest nie tylko zrozumienie podstawowych pojęć i języków programowania, ale także umiejętność korzystania z różnych narzędzi oraz rozwiązanie problemów na różnych poziomach. Jednym z takich przydatnych narzędzi jest "przetwarzanie argumentów wiersza poleceń" w języku PHP. W niniejszym artykule, dowiesz się dlaczego jest to ważne oraz jak skutecznie z tego skorzystać.

## Jak używać

Aby korzystać z przetwarzania argumentów wiersza poleceń w PHP, musimy najpierw ustawić odpowiednie zmienne. Przykładowo możemy użyć funkcji "getopt()" wraz z parametrem "a", aby odczytać argumenty wiersza poleceń w postaci tablicy asocjacyjnej. Następnie, możemy wyświetlić zawartość tej tablicy za pomocą funkcji "print_r()". Przykład kodu w języku PHP może wyglądać następująco:

```
<?php
// pobieramy argumenty wiersza poleceń i zapisujemy je w tablicy asocjacyjnej
$arguments = getopt("a:");

// wyświetlamy zawartość tablicy
print_r($arguments);
?>
```

Po uruchomieniu powyższego kodu, jeśli wiersz poleceń będzie zawierał na przykład argument "-a hello", to zostanie wyświetlone "Array ( [a] => hello)". W ten sposób, możemy w prosty sposób odczytać argumenty przekazane do programu wierszem poleceń.

## Deep Dive

Aby lepiej zrozumieć działanie przetwarzania argumentów wiersza poleceń w PHP, można prześledzić kilka kroków, które są wykonywane. Po pierwsze, argumenty są przekazywane do programu jako tekst, który musimy przetworzyć. Następnie, za pomocą funkcji "getopt()", tworzymy tablicę asocjacyjną, w której nazwy argumentów są kluczami, a wartościami są przekazane wartości. Dzięki temu, łatwo możemy odwołać się do konkretnych argumentów za pomocą ich nazw. W przypadku błędnego lub niepoprawnego argumentu, wartość klucza będzie równa "false". W ten sposób, możemy wykryć i obsłużyć ewentualne błędy w przekazywanych argumentach.

## Zobacz także

- [Oficjalna dokumentacja PHP - przetwarzanie argumentów wiersza poleceń](https://www.php.net/manual/en/function.getopt.php)
- [Tutorial na temat przetwarzania argumentów wiersza poleceń w PHP](https://www.tutorialspoint.com/php/php_command_line.htm)
- [Blog o programowaniu w języku PHP](https://php.pl/)