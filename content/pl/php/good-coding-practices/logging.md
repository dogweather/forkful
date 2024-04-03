---
date: 2024-01-26 01:07:08.374614-07:00
description: "Logowanie jest w zasadzie jak prowadzenie dziennika dla twojego kodu;\
  \ to czynno\u015B\u0107 polegaj\u0105ca na rejestrowaniu zdarze\u0144, b\u0142\u0119\
  d\xF3w i innych istotnych punkt\xF3w\u2026"
lastmod: '2024-03-13T22:44:35.503876-06:00'
model: gpt-4-1106-preview
summary: "Logowanie jest w zasadzie jak prowadzenie dziennika dla twojego kodu; to\
  \ czynno\u015B\u0107 polegaj\u0105ca na rejestrowaniu zdarze\u0144, b\u0142\u0119\
  d\xF3w i innych istotnych punkt\xF3w danych, kt\xF3re pojawiaj\u0105 si\u0119, gdy\
  \ aplikacja jest uruchamiana."
title: "Rejestrowanie zdarze\u0144"
weight: 17
---

## Jak to zrobić:
PHP posiada wbudowaną funkcję rejestrowania błędów, która jest łatwa w użyciu. Wystarczy umieścić `error_log()` w swoim kodzie, aby wysłać wiadomość do dzienników serwera. Można również dostosować ją do zapisywania w określonym pliku.

```php
<?php
// Logowanie prostej informacji
error_log("To jest wpis loga informacyjnego.");

// Logowanie komunikatu o błędzie
error_log("To jest wpis loga błędu.", 0);

// Logowanie do określonego pliku
file_put_contents('/ścieżka/do/twojego/custom.log', "Wpis własnego loga.\n", FILE_APPEND);

// Użycie Monolog dla strukturyzowanego logowania
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Stworzenie logera
$logger = new Logger('nazwa');
// Teraz dodajmy kilka handlerów
$logger->pushHandler(new StreamHandler('/ścieżka/do/twojego/monolog.log', Logger::WARNING));

// Teraz możesz użyć swojego logera
$logger->warning('To jest log ostrzeżenia!');
$logger->error('To jest log błędu!');
?>
```

To spowoduje wyjście twoich logów albo do dziennika serwera, albo do określonego pliku w formacie zwykłego tekstu.

## Szczegółowe omówienie:
Historycznie, programiści PHP polegali na funkcji `error_log()` lub logach Apache/Nginx, aby wyłapywać problemy, ale może to być chaotyczne z potrzebą parsowania zwykłych plików tekstowych i brakiem łatwego sposobu na filtrowanie lub sortowanie ich. Tu na scenę wchodzą biblioteki do logowania jak Monolog, które wprowadziły erę strukturyzowanego logowania w PHP. Te rozwiązania dają ci lepszą kontrolę, oferując wiele kanałów logowania, poziomy ważności oraz sformatowane wyjście (takie jak JSON, co jest marzeniem do programistycznego parsowania).

Alternatywy dla Monologa obejmują Log4php, KLogger i Apache's Log4php. Pod względem implementacji, solidne logowanie wymaga nie tylko zrzucania danych gdziekolwiek, ale rozważenia takich rzeczy jak rotacja logów, strategie archiwizacji oraz integracja z narzędziami monitorującymi, aby było naprawdę przydatne.

Powinieneś mieć na uwadze [PSR-3 Logger Interface](https://www.php-fig.org/psr/psr-3/), co określa wspólny interfejs dla bibliotek do logowania, zapewniając interoperacyjność i spójny sposób dostępu do mechanizmów logowania.

## Zobacz również:
- [Repozytorium GitHub Monolog](https://github.com/Seldaek/monolog)
- [Specyfikacja Interfejsu Loggera PSR-3](https://www.php-fig.org/psr/psr-3/)
- [Dokumentacja PHP Error Log](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: Prosta Klasa Logowania Dla PHP](https://github.com/katzgrau/KLogger)
- [Log4php: Wszechstronny framework logowania dla PHP](https://logging.apache.org/log4php/)

Zacznij od wbudowanych funkcji, ale dla bardziej zrównoważonego i skalowalnego podejścia, rozważ zainwestowanie czasu, aby zapoznać się z biblioteką taką jak Monolog. Szczęśliwego logowania!
