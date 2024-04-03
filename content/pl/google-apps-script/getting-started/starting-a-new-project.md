---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:14.944408-07:00
description: "Jak to zrobi\u0107: Aby rozpocz\u0105\u0107 nowy projekt w Google Apps\
  \ Script, masz kilka punkt\xF3w wej\u015Bcia, ale skupmy si\u0119 na najbardziej\
  \ bezpo\u015Bredniej metodzie:\u2026"
lastmod: '2024-03-13T22:44:34.903629-06:00'
model: gpt-4-0125-preview
summary: "Aby rozpocz\u0105\u0107 nowy projekt w Google Apps Script, masz kilka punkt\xF3\
  w wej\u015Bcia, ale skupmy si\u0119 na najbardziej bezpo\u015Bredniej metodzie."
title: Rozpoczynanie nowego projektu
weight: 1
---

## Jak to zrobić:
Aby rozpocząć nowy projekt w Google Apps Script, masz kilka punktów wejścia, ale skupmy się na najbardziej bezpośredniej metodzie: tworzeniu skryptu z Google Drive.

1. **Tworzenie projektu w Google Drive**
   - Przejdź do Google Drive (drive.google.com).
   - Kliknij "+ Nowy" > "Więcej" > "Google Apps Script".
   - Nowy projekt skryptu otworzy się w edytorze. Domyślnie zawiera plik `Code.gs` z przykładową funkcją `myFunction`.

2. **Konfiguracja projektu**
   - Zmień nazwę projektu dla jasności. Kliknij "Bez nazwy projekt" w lewym górnym rogu i nadaj mu znaczącą nazwę.
   - Napisz prostą funkcję w pliku `Code.gs`, aby się z nim zapoznać:

```javascript
function helloWorld() {
  Logger.log('Witaj, świecie!');
}
```

   - Uruchom `helloWorld`, wybierając funkcję z rozwijanego menu obok przycisku odtwarzania (▶) i klikając go. Spowoduje to wykonanie funkcji.

3. **Przeglądanie logów**
   - Aby zobaczyć wynik `Logger.log`, przejdź do "Widok" > "Logi" lub naciśnij `Ctrl + Enter`. Powinieneś zobaczyć "Witaj, świecie!" w logach.

Gratulacje, właśnie z sukcesem rozpocząłeś nowy projekt w Google Apps Script i uruchomiłeś prostą funkcję!

## Dogłębna analiza
Początki Google Apps Script około 2009 roku dostarczyły potężnej, a jednocześnie przystępnej platformy zarówno dla programistów, jak i osób nie programujących, aby automatyzować, rozszerzać i budować na bazie obszernej gamy usług Google. W przeciwieństwie do tradycyjnych środowisk programistycznych, GAS oferuje unikalne połączenie prostoty i integracji, bezpośrednio w ekosystemie Google, bez potrzeby zewnętrznych serwerów czy konfiguracji. Ten bezserwerowy model wykonania znacznie upraszcza wdrażanie i zarządzanie projektami.

Historycznie, GAS był w pewnym stopniu ograniczany przez swoje środowisko wykonawcze i wersję języka, często pozostając w tyle za aktualnymi standardami JavaScript. Jednakże, niedawne aktualizacje wprowadziły nowoczesną składnię JavaScript (ECMAScript 2015+) do GAS, czyniąc go bardziej przystępnym dla programistów przyzwyczajonych do współczesnych praktyk programistycznych.

Chociaż GAS jest unikalnie pozycjonowany do interakcji z usługami Google, istnieją alternatywne podejścia dla bardziej intensywnych lub specyficznych potrzeb. Na przykład, Google Cloud Functions i Google Cloud Platform (GCP) oferują bardziej solidne i skalowalne rozwiązania do obsługi skomplikowanych przepływów pracy, przetwarzania dużych zbiorów danych i integracji z zewnętrznymi API. Te platformy pozwalają na programowanie w różnych językach (np. Python, Go, Node.js) i oferują większe zasoby obliczeniowe.

Mimo to, dla zadań ściśle związanych z Google Apps, automatyzacją i szybkim rozwojem w tym ekosystemie, Google Apps Script pozostaje niezrównanym narzędziem pod względem łatwości użycia i głębokości integracji. Jego dostępność bezpośrednio z Google Drive i bezproblemowe połączenie z usługami Google czynią go praktycznym wyborem dla szerokiej gamy projektów, zwłaszcza dla tych, którzy chcą rozszerzyć funkcjonalność Arkuszy, Dokumentów, Formularzy i innych aplikacji Google.
