---
title:                "Korzystanie z interaktywnego shella (REPL)"
aliases: - /pl/swift/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:18:19.854126-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Korzystanie z interaktywnej powłoki, czyli pętli czytaj-wykonaj-drukuj (REPL), pozwala na interaktywne programowanie. Programiści używają jej do szybkiego testowania fragmentów kodu Swift, debugowania lub nauki języka.

## Jak to zrobić:
Wywołaj REPL, otwierając terminal i uruchamiając `swift`. Wpisz kod bezpośrednio i naciśnij Enter, aby go uruchomić. Oto przedsmak:

```Swift
1> let greeting = "Cześć, REPL!"
greeting: String = "Cześć, REPL!"
2> print(greeting)
Cześć, REPL!
```

Wyjdź za pomocą `:quit` lub `Control-D`.

## Wgłębiając się
Korzenie REPL sięgają interfejsów Lisp z lat 60. REPL Swifta opiera się na LLVM, potężnym frameworku kompilacyjnym, oferującym więcej niż tylko podstawową interpretację - to pełnoprawne narzędzie z autouzupełnianiem, debugowaniem i wieloma innymi. REPL jest świetny do nauki lub tworzenia prototypów, ale to nie jest samodzielne środowisko programistyczne. Niektórzy preferują używanie Playgrounds w Xcode dla bardziej graficznego, opartego na plikach podejścia, podczas gdy inni trzymają się tradycyjnego edytowania skryptów i ich uruchamiania.

Pod maską, REPL Swifta dynamicznie kompiluje kod do języka maszynowego i wykonuje go, co sprawia, że jest stosunkowo szybki. Może także uzyskać dostęp do dowolnych skompilowanych modułów Swift, a nawet bibliotek C, co czyni go dość potężnym. Należy jednak zauważyć, że nie wszystko działa doskonale w REPL; niektóre funkcje Swift, szczególnie te wymagające skomplikowanych ustawień projektu lub plików storyboard, tutaj nie zadziałają.

## Zobacz również
- [Swift.org - Rozpoczęcie](https://www.swift.org/getting-started/#using-the-repl)
- [Wprowadzenie do Playgrounds w Xcode](https://developer.apple.com/videos/play/wwdc2014/408/) Apple’a
- [Projekt LLVM](https://llvm.org/)
