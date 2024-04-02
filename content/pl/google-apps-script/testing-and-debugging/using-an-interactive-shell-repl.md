---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:09.681795-07:00
description: "Interaktywne \u015Brodowisko, czyli P\u0119tla Odczyt-Wykonanie-Wydruk\
  \ (REPL), to proste, interaktywne \u015Brodowisko programistyczne, kt\xF3re przyjmuje\
  \ pojedyncze\u2026"
lastmod: '2024-03-13T22:44:34.904742-06:00'
model: gpt-4-0125-preview
summary: "Interaktywne \u015Brodowisko, czyli P\u0119tla Odczyt-Wykonanie-Wydruk (REPL),\
  \ to proste, interaktywne \u015Brodowisko programistyczne, kt\xF3re przyjmuje pojedyncze\u2026"
title: "Korzystanie z interaktywnej pow\u0142oki (REPL)"
weight: 34
---

## Co i dlaczego?

Interaktywne środowisko, czyli Pętla Odczyt-Wykonanie-Wydruk (REPL), to proste, interaktywne środowisko programistyczne, które przyjmuje pojedyncze wejścia użytkownika (wyrażenia), ocenia je i zwraca wynik użytkownikowi. Programiści używają REPL do szybkiego prototypowania, debugowania oraz interaktywnej nauki składni i zachowania języka programowania.

## Jak to zrobić:

Google Apps Script, czyli chmurowy język skryptowy do automatyzacji zadań w produktach Google, nie ma wbudowanego narzędzia REPL podobnego do tych w językach takich jak Python czy JavaScript's Node.js. Jednak można symulować podobne doświadczenie, używając funkcji logowania i debugowania Edytora Apps Script lub ustawiając zewnętrzne środowisko. Tutaj skupimy się na tworzeniu prowizorycznego REPL w edytorze Apps Script.

1. **Tworzenie prowizorycznej funkcji REPL**:

```javascript
function myREPL() {
  var input = Logger.log('Wprowadź swoje wyrażenie: ');
  try {
    var result = eval(input);
    Logger.log('Wynik: ' + result);
  } catch(e) {
    Logger.log('Błąd: ' + e.message);
  }
}
```

Ponieważ bezpośrednie wejście użytkownika nie jest wykonalne w taki sam sposób, jak w tradycyjnym REPL w środowisku Apps Script, możesz ręcznie zmodyfikować zmienną `input` i uruchomić `myREPL()`, aby testować wyrażenia.

2. **Przykładowe wykonanie kodu**:

Załóżmy, że chcesz ocenić `2+2`. W tym celu zmodyfikujesz funkcję `myREPL` następująco:

```javascript
function myREPL() {
  var input = '2+2'; // Ręcznie wpisz swoje wyrażenie tutaj
  // Reszta pozostaje taka sama...
}
```

Po uruchomieniu `myREPL()`, sprawdź Logi (Widok > Logi) dla wyniku, który powinien brzmieć mniej więcej tak:

```
[20-xx-xxxx xx:xx:xx:xxx] Wprowadź swoje wyrażenie:
[20-xx-xxxx xx:xx:xx:xxx] Wynik: 4
```

3. **Debugowanie z Logger**:

Do bardziej złożonego debugowania, wpleć `Logger.log(zmienna);` w swój kod, aby drukować stany zmiennych, pomagając zrozumieć przepływ i pośrednie stany twoich skryptów.

## Głębsze spojrzenie

Koncepcja REPL jest głęboko zakorzeniona w historii informatyki, sięgając systemów dzielonego czasu z lat 60-tych, które umożliwiały interaktywne sesje. Języki takie jak Lisp rozkwitały w tym środowisku, ponieważ REPL był kluczowy dla ich iteracyjnego procesu rozwoju. W przeciwieństwie do tego, Google Apps Script, który pojawił się znacznie później, jest zaprojektowany głównie z myślą o sieci, koncentrując się na automatyzacji zadań w ramach zestawu Google bardziej niż na iteracyjnym, konsolowym programowaniu.

Google Apps Script tradycyjnie nie obsługuje w sposób natywny interaktywnych sesji kodowania w czasie rzeczywistym z powodu swojej chmurowej natury i skupienia na wdrożeniach aplikacji internetowych. Jego model wykonania obraca się wokół funkcji wyzwalanych przez zdarzenia sieciowe, czasowe wyzwalacze lub ręczne wywołanie w środowisku, a nie natychmiastowe pętle informacji zwrotnej dostarczane przez REPL.

Chociaż prowizoryczny REPL i debuger w edytorze Apps Script oferują pewien poziom interaktywności, nie replikują w pełni natychmiastowego feedbacku i efektywności tradycyjnych REPL znalezionych w wielu językach programowania. Deweloperzy szukający bardziej autentycznego doświadczenia REPL z technologiami Google mogą badać zewnętrzne środowiska JavaScript lub Node.js z API Google. Mogą one zapewnić bardziej responsywną i interaktywną sesję kodowania, choć wymagają więcej konfiguracji i potencjalnie wychodzenia poza bezpośrednie środowisko Apps Script.
