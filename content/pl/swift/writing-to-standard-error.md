---
title:    "Swift: Pisanie do standardowego błędu"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista musi radzić sobie z błędami w swoim kodzie. W tym artykule dowiesz się, dlaczego warto zapisać błędy do standardowego wyjścia błędów (standard error) i jak to zrobić w języku Swift.

## Jak to zrobić

Aby zapisać błędy do standardowego wyjścia błędów w Swift, musisz wykorzystać funkcję `fputs()`. Przyjmie ona jako parametry napis z treścią błędu oraz `stderr`, który jest wskaźnikiem na strumień błędów. Poniższy kod pokazuje przykład użycia tej funkcji:

```Swift
let errorMessage = "Wystąpił błąd w działaniu programu."
fputs(errorMessage, stderr)
```

Aby upewnić się, że błąd zostanie wyświetlony użytkownikowi, warto również wykorzystać instrukcję `exit(1)`, która zakończy działanie programu z kodem błędu. Poniższy kod jest pełnym przykładem obsługi błędu w języku Swift:

```Swift
func divide(_ x: Int, by y: Int) throws -> Int {
    guard y != 0 else {
        throw NSError(domain: "DivisionError", code: 1, userInfo: [NSLocalizedDescriptionKey: "Nie można dzielić przez 0."])
    }
    return x/y
}

do {
    let result = try divide(10, by: 0)
    print(result)
} catch {
    fputs(error.localizedDescription, stderr)
    exit(1)
}
```

W przypadku błędu w dzieleniu przez 0, funkcja `fputs()` wypisze odpowiedni komunikat na standardowym wyjściu błędów, a następnie program zakończy działanie z kodem błędu 1.

## Głębszy zanurk

Wykorzystanie standardowego wyjścia błędów jest szczególnie przydatne w przypadku większych projektów, gdzie może być konieczność obsługi wielu różnych typów błędów. W takich przypadkach warto stworzyć osobne typy dla różnych rodzajów błędów oraz wykorzystać instrukcję `do-catch` do ich obsługi.

## Zobacz także

- [Oficjalna dokumentacja Swift](https://swift.org/documentation/)
- [Podręcznik programisty w języku Swift](https://docs.swift.org/swift-book/)
- [Użycie NSError w celu obsługi błędów w języku Swift](https://developer.apple.com/documentation/foundation/nserror)
- [Inne sposoby obsługi błędów w Swift](https://medium.com/@julesthebot/handling-errors-in-swift-3-d9b91e4f9d0b)