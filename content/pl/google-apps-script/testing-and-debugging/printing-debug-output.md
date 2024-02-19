---
aliases:
- /pl/google-apps-script/printing-debug-output/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:08.294092-07:00
description: "Drukowanie wyj\u015Bcia debugowania polega na strategicznym umieszczaniu\
  \ w kodzie instrukcji logowania w celu wy\u015Bwietlenia warto\u015Bci zmiennych,\
  \ przep\u0142ywu\u2026"
lastmod: 2024-02-18 23:08:49.150450
model: gpt-4-0125-preview
summary: "Drukowanie wyj\u015Bcia debugowania polega na strategicznym umieszczaniu\
  \ w kodzie instrukcji logowania w celu wy\u015Bwietlenia warto\u015Bci zmiennych,\
  \ przep\u0142ywu\u2026"
title: "Drukowanie informacji wyj\u015Bciowych debugowania"
---

{{< edit_this_page >}}

## Co i dlaczego?

Drukowanie wyjścia debugowania polega na strategicznym umieszczaniu w kodzie instrukcji logowania w celu wyświetlenia wartości zmiennych, przepływu wykonania lub błędów komunikatów podczas uruchamiania. Programiści wykorzystują to intensywnie do śledzenia i diagnozowania zachowania ich skryptów, zapewniając poprawność i efektywność w swoich aplikacjach Google Apps Script.

## Jak to zrobić:

Google Apps Script zapewnia klasę `Logger` dla podstawowego debugowania, a dla bardziej zaawansowanych potrzeb, klasę `console` wprowadzoną w środowisku uruchomieniowym V8.

**Korzystanie z Logger:**

Klasa Logger umożliwia logowanie wiadomości debugowania, które można przeglądać po wykonaniu w edytorze Apps Script pod `Widok > Logi`. Oto prosty przykład:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Witaj, %s!", name);
}
```

Po uruchomieniu `logSample()`, można zobaczyć log ze "Witaj, Wired Reader!" w przeglądarce logów.

**Korzystanie z console.log w środowisku uruchomieniowym V8:**

W środowisku uruchomieniowym V8, `console.log` zapewnia bardziej znajomą składnię dla programistów pochodzących z innych języków:

```javascript
function consoleSample() {
  var status = 'aktywny';
  var count = 150;
  console.log(`Aktualny status: ${status}, Ilość: ${count}`);
}
```

Po wykonaniu, dostęp do Stackdriver Logging znajduje się w `Widok > Stackdriver Logging` w celu przeglądnięcia wyjścia. Jest to bardziej zaawansowane, wspierające interpolację ciągów i inspekcję obiektów, oraz integruje się z logowaniem Google Cloud, oferując trwałe logi i zaawansowane filtrowanie.

**Przykładowe wyjście z console.log:**

```
Aktualny status: aktywny, Ilość: 150
```

## Pogłębiona analiza

Początkowo `Logger.log` był głównym narzędziem do debugowania w Google Apps Script, oferując prosty, bezpośredni sposób na drukowanie wyjścia do inspekcji. Jednakże, w miarę jak skrypty stawały się bardziej złożone i integrowane z usługami Google Cloud Platform, potrzeba bardziej solidnego rozwiązania do logowania stała się oczywista.

Wprowadzenie środowiska uruchomieniowego V8 przyniosło `console.log` do użytku. Nie tylko dostosowuje Google Apps Script do standardowej składni JavaScript, czyniąc język bardziej dostępnym dla programistów znających JavaScript, ale także wykorzystuje potężną infrastrukturę logowania Google Cloud. Wprowadzenie `console.log` i jego integracja z Google Cloud Platform oznacza znaczną ewolucję w zakresie możliwości debugowania w Google Apps Script, zapewniając programistom bardziej dynamiczne i skalowalne podejście do monitorowania i rozwiązywania problemów ze swoimi skryptami.

Chociaż `Logger.log` wystarcza do podstawowych potrzeb debugowania i małych projektów, `console.log` ze środowiskiem uruchomieniowym V8 oferuje bardziej wszechstronne i przyszłościowe rozwiązanie. Obejmuje to możliwość zachowania logów poza sesją wykonania, wyszukiwania i filtrowania logów w konsoli Google Cloud oraz ogólne dostosowanie do nowoczesnych praktyk rozwoju JavaScript. Jednak programiści powinni ocenić swoje potrzeby w kontekście złożoności i skali swoich projektów, wybierając między tymi opcjami.
