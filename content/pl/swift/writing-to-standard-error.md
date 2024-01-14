---
title:                "Swift: Pisanie do standardowego wyjścia błędu"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Dlaczego pisać do standardowego błędu w Swift?

Czasami w trakcie pisania kodu w Swift może pojawić się potrzeba skierowania błędu do standardowego wyjścia. Może to być przydatne w celu zdiagnozowania problemów lub wyświetlenia ważnych informacji dla użytkownika. W tym artykule dowiesz się, dlaczego warto nauczyć się pisać do standardowego błędu oraz jak to zrobić w praktyce.

## Jak to zrobić?

Aby pisać do standardowego błędu w Swift, możesz wykorzystać funkcję `print()` z parametrem `toStream`. Należy przekazać jej wartość `stderr`, co oznacza standardowe wyjście błędów. Poniższy kod pokazuje prosty przykład użycia tej funkcji:

```Swift
print("Nie udało się załadować pliku.", toStream: &stderr)
```

Będzie ona wyświetlać tekst w standardowym formacie błędu, czyli z prefiksem `error:`.

```
error: Nie udało się załadować pliku.
```

Możesz również skorzystać ze struktury `FileHandle`, która udostępnia dostęp do standardowych strumieni, w tym do standardowego wyjścia błędów. Poniższy przykład używa `FileHandle` do zapisywania danych do standardowego wyjścia błędów:

```Swift
let errorOutput = FileHandle.standardError
let errorData = "Wystąpił błąd".data(using: .utf8)
errorOutput.write(errorData!)
```

Ten sposób może być szczególnie przydatny, gdy chcesz przekazać do standardowego błędu więcej niż tylko tekstu.

## Głębszy zanurzenie

Pisanie do standardowego błędu może być również pomocne w przypadku korzystania z bibliotek zewnętrznych, które nie wyświetlają błędów w konsoli lub nie mają opcji raportowania ich w inny sposób. Wtedy możesz samodzielnie wyświetlić błąd w standardowym wyjściu błędów, aby zobaczyć, co jest nieprawidłowe.

## Zobacz także

- Dokumentacja Apple na temat funkcji `print()` w języku Swift: [https://developer.apple.com/documentation/swift/1541053-print](https://developer.apple.com/documentation/swift/1541053-print)
- Dokumentacja Apple na temat struktury `FileHandle` w języku Swift: [https://developer.apple.com/documentation/foundation/filehandle](https://developer.apple.com/documentation/foundation/filehandle)
- Wideo z YouTube na temat pisania do standardowego błędu w Swift: [https://www.youtube.com/watch?v=CRv4FMmnG24](https://www.youtube.com/watch?v=CRv4FMmnG24)