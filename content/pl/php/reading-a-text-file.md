---
title:                "PHP: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub chcesz nim zostać, to na pewno zetknąłeś się z czytaniem plików tekstowych w swoich projektach. Bez względu na to czy pracujesz z danymi, tekstami czy kodem, prawdopodobnie często musisz odczytać informacje z pliku. Dlatego warto dowiedzieć się, jak to zrobić w PHP.

## Jak to zrobić

W PHP istnieje kilka sposobów na odczytanie pliku tekstowego. Jednym z nich jest użycie funkcji "file_get_contents()". Przykładowo, chcąc odczytać plik o nazwie "dane.txt", należy użyć następującego kodu:

```PHP
$plik = "dane.txt";
$zawartosc = file_get_contents($plik);

echo $zawartosc;
```

Wywołanie funkcji "file_get_contents()" zwróci całą zawartość pliku jako string. Możesz również odczytać plik linia po linii używając funkcji "fgets()". Przykładowo, chcąc wyświetlić kolejno wszystkie linie z pliku, należy użyć poniższego kodu:

```PHP
$plik = fopen("dane.txt", "r");

while(!feof($plik)) {
  $linia = fgets($plik);
  echo $linia;
}

fclose($plik);
```

## Głębsze zagadnienia

Podczas pracy z plikami tekstowymi w PHP ważne jest zapewnienie bezpieczeństwa. Aby mieć pewność, że obsługujesz poprawny plik, zawsze sprawdzaj czy plik istnieje i czy masz do niego dostęp. Możesz to zrobić przy użyciu funkcji "file_exists()" oraz "is_readable()".

W przypadku, gdy plik jest bardzo duży, dobrą praktyką jest użycie funkcji "fopen()" z parametrem "r+", który pozwala na jednoczesną możliwość odczytu i zapisu do pliku. Pozwala to na bardziej efektywne operacje, szczególnie w przypadku pracy na serwerze.

## Zobacz również

- [Dokumentacja PHP o funkcji "file_get_contents()"](https://www.php.net/manual/en/function.file-get-contents.php)
- [Dokumentacja PHP o funkcji "fgets()"](https://www.php.net/manual/en/function.fgets.php)
- [Notatnik programisty - czytanie plików tekstowych w PHP](https://notatnik-programisty.pl/php/czytanie-plikow-tekstowych-w-php/)