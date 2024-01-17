---
title:                "Łączenie ciągów znaków"
html_title:           "PHP: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konkatynacja ciągów w programowaniu oznacza łączenie kilku łańcuchów znaków w jeden dłuższy ciąg. Programiści często stosują tę technikę, aby ułatwić sobie manipulację i przetwarzanie danych tekstowych.

## Jak to zrobić:
Przedstawię teraz kilka praktycznych przykładów pokazujących, jak skorzystać z konkatynacji ciągów w PHP. Wszystkie przykłady będą używać wbudowanej funkcji `concat()`. 

### Przykład 1:
```PHP
$imie = 'Kasia';
$nazwisko = 'Kowalska';
$login = concat($imie, $nazwisko);

echo $login; // wyświetli 'KasiaKowalska'
```

### Przykład 2:
```PHP
$data = '30-09-2021';
$godzina = '18:00';
$wydarzenie = 'Spotkanie zespołu';
$opis = concat('Dzisiaj o', $godzina, 'odbędzie się', $wydarzenie, 'o temacie', $data);

echo $opis; // wyświetli 'Dzisiaj o 18:00 odbędzie się Spotkanie zespołu o temacie 30-09-2021'
```

### Przykład 3:
```PHP
$adres = 'http://www.example.com/';
$sekcja = 'blog';
$id = 123;
$artykul = 'Nowe wpisy na naszym blogu: ' . $adres . $sekcja . '?id=' . $id;

echo $artykul; // wyświetli 'Nowe wpisy na naszym blogu: http://www.example.com/blog?id=123'
```

## Głębszy wgląd:
Ten sposób łączenia ciągów znaków istnieje w wielu językach programowania. W PHP wykorzystuje się funkcję `concat()`, jednak w innych językach możemy spotkać bardziej znany operator `+` lub inny sposób zapisu. Ważne jest, aby pamiętać o połączeniu każdego elementu oddzielnie, a także o odpowiednim sposobie zapisu zmiennych, aby uniknąć ewentualnych błędów. Można również wykorzystać funkcję `sprintf()` aby lepiej kontrolować format wyjścia.

## Zobacz też:
Dowiedz się więcej o konkatynacji ciągów w PHP na stronie dokumentacji języka: https://www.php.net/manual/en/function.concat.php