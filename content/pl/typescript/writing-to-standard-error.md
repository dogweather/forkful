---
title:                "Pisanie do standardowego błędu"
html_title:           "TypeScript: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?
Pisanie do standardowego wyjścia błędu (ang. standard error) jest często wykonywanym działaniem przez programistów. Jest to po prostu jedno z wielu wyjść, które aplikacja może użyć do wyświetlenia informacji o ewentualnych błędach lub ostrzeżeniach. Często używane w celu przekazania ważnych informacji o działaniu aplikacji.

# Jak to zrobić:
W TypeScript istnieje specjalny obiekt o nazwie `console`, który umożliwia wypisywanie do różnych wyjść, w tym do standardowego wyjścia błędu. Należy użyć metody `error()` tego obiektu, przekazując jako argument wiadomość, którą chcemy wyświetlić. Następnie używając polecenia `node` możemy uruchomić nasz plik TypeScript i sprawdzić, czy wiadomość została wyświetlona poprawnie. Poniższy przykład ilustruje ten proces:

```TypeScript
console.error("To jest wiadomość o błędzie!");
```
Output:
```
To jest wiadomość o błędzie!
```

# Głębsze zagadnienia:
Pisanie do standardowego wyjścia błędu jest stosowane od bardzo dawna w programowaniu. W starszych językach takich jak C czy Java, jedynym dostępnym wyjściem było właśnie standardowe wyjście błędu. W przypadku TypeScript istnieje również możliwość przekierowania standardowego wyjścia błędu do innego miejsca, na przykład do pliku, dzięki temu możliwe jest zapisywanie i analizowanie błędów na późniejszym etapie. Alternatywnym sposobem reagowania na błędy jest rzucanie wyjątków, jednak pisanie do standardowego wyjścia błędu jest szybkim i prostym sposobem na szybkie zwrócenie uwagi na potencjalne problemy w aplikacji.

# Zobacz też:
- Dokumentacja obiektu `console`: https://developer.mozilla.org/pl/docs/Web/API/Console
- Porównanie pomiędzy pisaniem do standardowego wyjścia błędu a rzucaniem wyjątków: https://stackoverflow.com/questions/2127312/why-should-exceptions-be-used-sparingly
- Inne metody reagowania na błędy w TypeScript: https://www.typescriptlang.org/docs/handbook/error-handling.html