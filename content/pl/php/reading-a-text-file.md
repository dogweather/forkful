---
title:    "PHP: Odczytywanie pliku tekstowego"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego
Często podczas pracowania z PHP, konieczne jest odczytywanie danych z plików tekstowych. Może to być lista użytkowników, ustawień aplikacji lub innych istotnych informacji. W tym artykule dowiesz się, jak w prosty sposób odczytywać pliki tekstowe za pomocą PHP.

## Jak to zrobić
Do odczytywania plików tekstowych w PHP używana jest funkcja `file_get_contents()`. Przyjmuje ona jako argument ścieżkę do pliku i zwraca jego zawartość w postaci ciągu znaków. W przykładzie poniżej odczytamy plik "users.txt" i wyświetlimy jego zawartość na stronie:
```PHP
$file = 'users.txt';
$users = file_get_contents($file);

echo $users; // wyświetli zawartość pliku
```

Gdy plik zawiera zestaw danych oddzielonych znakiem nowej linii, możemy również użyć funkcji `file()` do odczytania pliku linia po linii. W przykładzie poniżej wyświetlimy listę użytkowników na stronie, każdego w nowym wierszu:
```PHP
$file = 'users.txt';
$users = file($file);

foreach($users as $user) {
  echo $user . '<br>';
}

// wyjście:
// Jan Kowalski
// Anna Nowak
// Piotr Czerniak
```

Jeśli chcemy odczytać plik w celu późniejszego przetworzenia jego zawartości, możemy użyć funkcji `fopen()` do otwarcia pliku w trybie odczytu, a następnie użyć funkcji `fgets()` do odczytania kolejnych linii. Przykład poniżej wyświetli pierwszą linię pliku i zamknie go po zakończeniu:
```PHP
$file = fopen('users.txt', 'r');
$user = fgets($file);
fclose($file);

echo $user; // wyświetli pierwszą linię pliku
```

## Deep Dive
Odczytywanie plików tekstowych za pomocą funkcji `file()` i `file_get_contents()` może się okazać bardzo wydajne i wygodne, jednak warto pamiętać o kilku ważnych rzeczach:

- Podczas odczytywania większych plików, należy użyć funkcji `file()` zamiast `file_get_contents()` aby uniknąć zapełnienia pamięci dostępnej dla PHP.
- Należy pamiętać o prawidłowej obsłudze znaków specjalnych, takich jak znak nowej linii.
- W przypadku odczytywania plików, które mogą zawierać poufne dane, należy zabezpieczyć się przed dostępem nieautoryzowanych osób za pomocą funkcji `readfile()` lub odpowiednich uprawnień do pliku.

## Zobacz również
- [Oficjalna dokumentacja PHP: file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [Oficjalna dokumentacja PHP: file()](https://www.php.net/manual/en/function.file.php)
- [Oficjalna dokumentacja PHP: fopen()](https://www.php.net/manual/en/function.fopen.php)
- [Oficjalna dokumentacja PHP: fgets()](https://www.php.net/manual/en/function.fgets.php)
- [Oficjalna dokumentacja PHP: readfile()](https://www.php.net/manual/en/function.readfile.php)