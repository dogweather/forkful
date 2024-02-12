---
title:                "Rejestrowanie zdarzeń"
aliases:
- /pl/python/logging.md
date:                  2024-01-26T01:09:09.622104-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Logowanie to proces rejestrowania zdarzeń aplikacji, podczas działania programu, dostarczając ślad okruszków do analizy post mortem oraz monitorowania w czasie rzeczywistym. Programiści robią to, ponieważ pomaga to w debugowaniu problemów, monitorowaniu wydajności oraz śledzeniu działań użytkowników dla celów bezpieczeństwa i analizy.

## Jak to zrobić:
Python posiada wbudowany moduł do logowania. Oto podstawowa konfiguracja:
```Python
import logging

# Podstawowa konfiguracja logowania
logging.basicConfig(level=logging.INFO)

# Wiadomości logowania
logging.debug('To jest wiadomość debugowania')
logging.info('Informacje o tym, co właśnie zrobił twój program')
logging.warning('Wiadomość ostrzegawcza')
logging.error('Wystąpił błąd')
logging.critical('Program nie jest w stanie się odzyskać!')
```
Kiedy uruchomisz ten kod, zobaczysz następujące wyjście (ponieważ domyślny poziom to OSTRZEŻENIE, wiadomości debugowania i informacyjne nie będą wyświetlane):
```
WARNING:root:Wiadomość ostrzegawcza
ERROR:root:Wystąpił błąd
CRITICAL:root:Program nie jest w stanie się odzyskać!
```
Możesz również skonfigurować logowanie tak, aby zapisywało do pliku zamiast na konsolę:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Teraz twoje logi będą kierowane do pliku 'app.log'.

## Szczegółowa analiza
Logowanie istnieje od pierwszych dni programowania, przy czym system logowania jest jedną z najstarszych form trwałego przechowywania danych poza faktycznymi plikami z danymi. Pomijając historię, główna koncepcja logowania pozostaje w zasadzie niezmieniona, chociaż narzędzia ewoluowały.

Moduł `logging` w Pythonie jest dość potężny i elastyczny. Pozwala programistom na ustawienie różnych poziomów logowania (DEBUG, INFO, WARNING, ERROR, CRITICAL), które mogą pomóc w kategoryzowaniu i filtrowaniu logów. Posiada hierarchiczny system loggerów, co oznacza, że możesz mieć relacje rodzic-dziecko między loggerami i propagować wiadomości w górę łańcucha.

Alternatywami są biblioteki stron trzecich jak Loguru czy structlog, które oferują zaawansowane funkcje i prostszy interfejs niż wbudowany moduł logowania. Mogą zapewnić ładniejsze wyjście, lepszą serializację strukturalnych danych oraz bardziej intuicyjne sposoby radzenia sobie z konfiguracją logowania.

Jeśli chodzi o wdrożenie, przy konfiguracji logowania ważne jest, aby zrobić to jednokrotnie na początku aplikacji. Zaleca się konfigurowanie na poziomie modułu za pomocą `logging.getLogger(__name__)`, aby być zgodnym z najlepszymi praktykami logowania w Pythonie.

Logowanie nie powinno drastycznie wpływać na wydajność aplikacji w normalnych okolicznościach. Jednak należy uważać na to, co jest logowane: zbyt rozbudowane logowanie, szczególnie na poziomach DEBUG, może spowolnić aplikację i szybko zapełnić pamięć przechowywania plików logów.

## Zobacz również
Aby dowiedzieć się więcej o module logowania Pythona, sprawdź oficjalną książkę kucharską logowania Pythona dla kilku świetnych przykładów i najlepszych praktyk: https://docs.python.org/3/howto/logging-cookbook.html

Aby uzyskać szczegółowe informacje na temat strukturalnego logowania i tego, jak może pomóc uczynić logi bardziej informatywnymi i łatwiejszymi do analizy, Loguru jest dobrze udokumentowany: https://loguru.readthedocs.io

Rozważ również przyjrzenie się metodologii 12-factor app, szczególnie sekcji dotyczącej logów dla nowoczesnego poglądu na logowanie aplikacji: https://12factor.net/logs
