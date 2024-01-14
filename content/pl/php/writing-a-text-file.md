---
title:    "PHP: Tworzenie pliku tekstowego"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią procesu programowania w PHP. Jest to przydatna umiejętność, która pozwala na zapisywanie i odczytywanie danych w plikach, co jest niezbędne w wielu aplikacjach internetowych. W tym artykule dowiecie się, w jaki sposób można to zrobić w PHP.

## Jak to zrobić

W przypadku pisania pliku tekstowego w PHP należy wykorzystać funkcję "fopen()", która otwiera plik do zapisu lub odczytu, w zależności od potrzeb. Ważne jest, aby podać ścieżkę do pliku oraz tryb otwarcia, który określa, czy plik będzie otwierany do zapisu czy odczytu.

```
$plik = fopen("twój_plik.txt", "w"); //otwarcie pliku do zapisu
```

Następnie możesz użyć funkcji "fwrite()", aby zapisać dane do pliku. Ta funkcja przyjmuje jako argumenty uchwyt do otwartego pliku oraz dane, które chcesz zapisać. Poniżej znajduje się przykład zapisu napisu "Witaj świecie!" do pliku.

```
fwrite($plik, "Witaj świecie!");
```

Po użyciu funkcji "fwrite()" zawsze należy zamknąć otwarty plik, korzystając z funkcji "fclose()".

```
fclose($plik); //zamknięcie pliku
```

Aby odczytać dane z pliku, należy użyć funkcji "fread()", która przyjmuje jako argumenty uchwyt do pliku oraz liczbę bajtów, którą chcesz odczytać. Poniższy przykład odczyta całą zawartość pliku i wyświetli ją na ekranie.

```
$plik = fopen("twój_plik.txt", "r"); //otwarcie pliku do odczytu
echo fread($plik, filesize("twój_plik.txt")); //wyswietlenie zawartości pliku
fclose($plik); //zamknięcie pliku
```

## Głębszy wgląd

Podczas pisania pliku tekstowego, istnieje także możliwość ustawienia dodatkowych opcji, takich jak kodowanie tekstu czy tryb dostępu. Możesz również wykorzystać funkcję "file_put_contents()" do zapisania danych do pliku jedną linijką kodu.

Ważne jest również, aby upewnić się, że plik jest otwierany i zamykany w odpowiedniej sekcji programu, aby uniknąć problemów z dostępem do pliku przez inne procesy.

## Zobacz również

1. [Dokumentacja PHP: fopen()](https://www.php.net/manual/en/function.fopen.php)
2. [Dokumentacja PHP: fwrite()](https://www.php.net/manual/en/function.fwrite.php)
3. [Dokumentacja PHP: fclose()](https://www.php.net/manual/en/function.fclose.php)
4. [Dokumentacja PHP: file_put_contents()](https://www.php.net/manual/en/function.file-put-contents.php)