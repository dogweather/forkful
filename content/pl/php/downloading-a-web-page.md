---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej to proces ściągania wszystkich jej danych potraktowanych jako plik. Programiści robią to, aby np. analizować strukturę strony, przeszukiwać informacje czy testować szybkość ładowania.

## Jak to zrobić:

Możemy użyć do tego celu wbudowanej funkcji w PHP: `file_get_contents`. Oto jak to zrobić:

```PHP
$url = 'http://nasza-strona.pl';
$strona = file_get_contents($url);
echo $strona;
```

W ten prosty sposób pobieramy i wyświetlamy zawartość strony internetowej.

## Głębiej:

Historia pobierania stron sięga początków samego internetu, kiedy to wszystko było tekstem i różne strony były ściągane dla analizy lub przeglądania offline. Dziś technologia poszła dalej, ale podstawowy proces pozostał ten sam.

Istnieją alternatywy dla `file_get_contents`, takie jak cURL, który daje więcej możliwości konfiguracji i lepiej radzi sobie z błędami.

Omawiana funkcja `file_get_contents` działa poprzez nawiązanie połączenia z serwerem, wysłanie żądania HTTP GET i zwrócenie odpowiedzi jako string. W przypadku błędu zwraca FALSE.

## Zobacz również:

Najlepszym miejscem do dalszego zgłębiania tematu jest oficjalna dokumentacja PHP. Znajdziesz tam więcej informacji na temat `file_get_contents` i o wiele więcej:
- [file_get_contents](https://www.php.net/manual/pl/function.file-get-contents.php)
- [cURL](https://www.php.net/manual/pl/book.curl.php)

Jeśli chcesz dowiedzieć się więcej o HTTP, polecam dokumentacje Mozilla Developer Network:
- [Zrozumienie HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP/Overview)

Powodzenia z kodowaniem w PHP!