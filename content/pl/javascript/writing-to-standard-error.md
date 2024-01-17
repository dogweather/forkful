---
title:                "Pisanie do standardowego błędu"
html_title:           "Javascript: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Wypisywanie do standardowego błędu jest często wykorzystywaną praktyką w programowaniu. Polega ona na wypisywaniu komunikatów o błędach lub ostrzeżeniach w kodzie, które są później wyświetlane w oknie konsoli. Programiści stosują to, aby ułatwić sobie debugowanie i odnajdywanie problemów w swoim kodzie.

## Jak to zrobić:
Przykładowy kod wypisania do standardowego błędu wygląda następująco:
```Javascript
console.error("To jest przykładowy komunikat o błędzie.");
```
Po uruchomieniu kodu powyżej, w oknie konsoli zostanie wyświetlona taka informacja:
```
To jest przykładowy komunikat o błędzie.
```
W ten sposób programista może szybko zauważyć, że w kodzie znajduje się pewien błąd i przystąpić do jego naprawy.

## Zagłębienie:
Wypisywanie do standardowego błędu jest częścią standardu języka Javascript. Jest to alternatywa dla wypisywania do standardowego wyjścia (console.log), które jest częściej wykorzystywane do wyświetlania informacji o działaniu programu. Wypisywanie do standardowego błędu można też wykorzystać do monitorowania wydajności aplikacji lub raportowania szczegółowych informacji o błędach.

## Zobacz też:
[Dokumentacja Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/console#error)