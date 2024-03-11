---
date: 2024-01-26 04:11:28.219967-07:00
description: "Debugger to narz\u0119dzie, kt\xF3re pozwala bada\u0107 i modyfikowa\u0107\
  \ wewn\u0119trzne dzia\u0142anie kodu podczas jego wykonywania. Programi\u015Bci\
  \ u\u017Cywaj\u0105 go do znajdowania i\u2026"
lastmod: '2024-03-11T00:14:08.317122-06:00'
model: gpt-4-0125-preview
summary: "Debugger to narz\u0119dzie, kt\xF3re pozwala bada\u0107 i modyfikowa\u0107\
  \ wewn\u0119trzne dzia\u0142anie kodu podczas jego wykonywania. Programi\u015Bci\
  \ u\u017Cywaj\u0105 go do znajdowania i\u2026"
title: Korzystanie z debugera
---

{{< edit_this_page >}}

## Co i dlaczego?
Debugger to narzędzie, które pozwala badać i modyfikować wewnętrzne działanie kodu podczas jego wykonywania. Programiści używają go do znajdowania i naprawiania błędów poprzez krokowe przeglądanie kodu, inspekcję zmiennych i zrozumienie przepływu ich programu.

## Jak korzystać:

Aby zacząć pracę z debuggerem w TypeScript, wszystko, czego potrzebujesz to obsługiwane IDE (takie jak Visual Studio Code) i konfiguracja `launch.json`. Oto szybki przykład dla aplikacji Node.js:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Cześć, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

Aby zdebugować to, utwórz plik `launch.json` w folderze `.vscode`:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Uruchom program",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Następnie, ustaw punkt przerwania (breakpoint) w funkcji `greet`, klikając po lewej stronie numeru linii w swoim IDE. Naciśnij F5, aby rozpocząć debugowanie, i obserwuj, jak aplikacja zatrzymuje się na punkcie przerwania. Teraz możesz najechać kursorem na zmienne, obserwować wyrażenia i krokowo przechodzić przez kod z łatwością.

## Głębsze zanurzenie

Kiedyś, zanim zintegrowane środowiska programistyczne (IDE) stały się zaawansowane, debugowanie często odbywało się za pomocą wypisów (tzw. debugowanie za pomocą `console.log`). To działało, jako tako, ale było jak szukanie igły w stogu siana na ślepo.

Nowoczesne debuggery to jak scyzoryk szwajcarski do rozwiązywania problemów. Wraz z ewolucją TypeScript i Node.js, dostępnych jest wiele debuggerów, od wbudowanego inspektora Node.js po narzędzia deweloperskie przeglądarek dla debugowania kodu po stronie klienta.

Inspektor Node.js działa poprzez dołączenie do uruchomionej aplikacji; komunikuje się za pomocą protokołu Chrome DevTools, przekształcając przeglądarkę Chrome w potężną konsolę debugowania. Ta integracja umożliwia wizualnie interaktywną i szczegółową sesję debugowania, w porównaniu do tradycyjnych praktyk debugowania z linii poleceń.

## Zobacz również

Dla małego dodatkowego czytania i kilku profesjonalnych wskazówek, sprawdź:

- [Debugowanie TypeScript w Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Przewodnik po debugowaniu Node.js](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Dokumentacja Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
