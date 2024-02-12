---
title:                "Korzystanie z interaktywnego shella (REPL)"
aliases:
- /pl/typescript/using-an-interactive-shell-repl/
date:                  2024-01-26T04:18:54.124150-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Read-Eval-Print-Loop (REPL) to środowisko programistyczne, które przyjmuje pojedyncze wejścia od użytkownika, wykonuje je i zwraca wynik użytkownikowi. Programiści używają REPL do szybkiego eksperymentowania ze skrawkami kodu, debugowania i uczenia się nowych funkcji języka bez konieczności tworzenia pełnej aplikacji.

## Jak to zrobić:
TypeScript nie ma własnego REPL. Użyjmy `ts-node`, środowiska wykonawczego TypeScript dla Node.js, które zawiera REPL.

Najpierw zainstaluj go globalnie:
```bash
npm install -g ts-node
```

Uruchom REPL, wpisując `ts-node` w wierszu poleceń:
```bash
ts-node
```

Oto krótki fragment kodu do wypróbowania:
```TypeScript
> let message: string = 'Cześć, REPL!';
> console.log(message);
Cześć, REPL!
> 
```
Aby zakończyć sesję, naciśnij `Ctrl+D`.

## Pogłębione informacje
Historycznie, REPL był wyróżniającym elementem języków takich jak Lisp, umożliwiając dynamiczną ewaluację kodu. Koncepcja ta od tego czasu się rozprzestrzeniła, stając się podstawą do interaktywnego kodowania w wielu językach.

Dla TypeScript, `ts-node` to nie jedyna opcja. Alternatywy obejmują używanie TypeScript Playground w przeglądarce internetowej lub korzystanie z innych REPL opartych na Node.js, które obsługują TypeScript z odpowiednimi wtyczkami.

Pod względem implementacji, `ts-node` używa API kompilatora TypeScript do transkompilacji kodu "na bieżąco", zanim zostanie on wykonany przez Node.js. Daje to natychmiastową informację zwrotną i jest szczególnie użyteczne do wypróbowania najnowszych funkcji TypeScript bez problemów z konfiguracją.

Warto pamiętać – choć REPL jest świetny do szybkich testów, nie zastępuje pisania tradycyjnego, testowalnego i utrzymywalnego kodu. Jest narzędziem do nauki i eksploracji, a nie substytutem dla odpowiednich praktyk programistycznych.

## Zobacz także
- [Oficjalna strona TypeScript](https://www.typescriptlang.org/)
- [ts-node na GitHubie](https://github.com/TypeStrong/ts-node)
- [Dokumentacja REPL Node.js](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)
