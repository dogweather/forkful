---
title:                "Pobieranie strony internetowej"
html_title:           "PHP: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Co i dlaczego?

Pobieranie stron internetowych to proces pobierania danych z serwera internetowego i wyświetlenia ich na ekranie użytkownika. Programiści często wykonują tę czynność, aby uzyskać potrzebne informacje lub dane z innych stron internetowych.

##Jak to zrobić:

Pobieranie strony internetowej w PHP jest łatwe i wymaga tylko kilku prostych kroków. Najpierw musimy użyć funkcji `file_get_contents` z odpowiednim adresem URL, aby pobrać treść strony. Następnie możemy użyć funkcji `echo` do wyświetlenia pobranego tekstu na ekranie. Na przykład:

```PHP
$strona = file_get_contents("https://www.example.com");
echo $strona;
```

To polecenie pobierze zawartość strony internetowej z adresu URL "https://www.example.com" i wyświetli ją na ekranie.

Możemy także użyć funkcji `file_put_contents` do zapisania pobranego tekstu do pliku na naszym serwerze. Na przykład:

```PHP
$strona = file_get_contents("https://www.example.com");
file_put_contents("nazwa_pliku.html", $strona);
```

Ten kod spowoduje pobranie strony z adresu URL "https://www.example.com" i zapisze ją jako plik "nazwa_pliku.html" na naszym serwerze.

##Głębsza Analiza:

Pobieranie stron internetowych jest powszechnie wykorzystywaną techniką w PHP, szczególnie w kontekście zautomatyzowania procesów i pozyskiwania danych. Niektóre alternatywne metody to użycie biblioteki cURL lub funkcji `fopen` wraz z `fread`, jednak funkcja `file_get_contents` jest popularnym i prostszym wyborem.

Podczas pobierania stron internetowych ważne jest również uważać na możliwe błędy związane z niedostępną lub błędną stroną. Dlatego warto dodać obsługę wyjątków do naszego kodu i sprawdzić poprawność adresu URL przed próbą pobrania strony.

##Zobacz również:

Jeśli chcesz dowiedzieć się więcej o funkcji `file_get_contents` w PHP, możesz znaleźć pomocne informacje w poniższych źródłach:

- Dokumentacja PHP: https://www.php.net/manual/en/function.file-get-contents.php
- Wideo na YouTube o pobieraniu stron internetowych w PHP: https://www.youtube.com/watch?v=m68_qUn-Prw
- Poradnik na Codecourse o pobieraniu zawartości stron internetowych w PHP: https://www.codecourse.com/lessons/automating-tasks-in-php/995

Wykorzystanie tej funkcji może być bardzo przydatne przy tworzeniu skryptów do automatyzacji różnych działań związanych z pobieraniem danych z internetu. Dlatego warto poświęcić trochę czasu na dokładniejsze poznanie jej działania.