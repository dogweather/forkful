---
title:    "TypeScript: Zaczynanie nowego projektu"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego:

Niejednokrotnie zdarza się, że jako programiści jesteśmy zmuszeni do rozpoczęcia nowego projektu. Może to być wymóg ze strony naszego pracodawcy, ale może także wynikać z naszej chęci rozwoju i tworzenia czegoś nowego. Bez względu na powody, warto wiedzieć, jak zacząć nowy projekt w języku TypeScript.

## Jak to zrobić:

Pierwszym krokiem do rozpoczęcia nowego projektu jest oczywiście instalacja TypeScript. Możemy to zrobić za pomocą `npm` lub `yarn` poprzez wywołanie poleceń `npm install typescript` lub `yarn add typescript`.

Kolejnym krokiem jest utworzenie nowego pliku `tsconfig.json`, w którym będziemy ustawiać konfigurację dla naszego projektu. W tym pliku możemy zmienić ustawienia dotyczące kompilacji, includować i excludować konkretne pliki oraz ustawić target, na który ma być skompilowany nasz projekt.

Teraz możemy już utworzyć nasz pierwszy plik z rozszerzeniem `.ts` i rozpocząć pisanie kodu w języku TypeScript. Poniżej znajduje się przykład prostego programu, który dodaje dwie liczby i wyświetla wynik w konsoli:

```typescript
let a: number = 5;
let b: number = 10;
let result: number = a + b;

console.log(`Wynik dodawania to: ${result}`);
```

Po napisaniu kodu, musimy go skompilować za pomocą komendy `tsc nazwa_pliku.ts`. W ten sposób utworzony zostanie plik `.js`, który możemy uruchomić za pomocą komendy `node nazwa_pliku.js`. W konsoli powinien pojawić się wynik naszej operacji.

## Deep Dive:

Przygotowanie odpowiedniego środowiska i konfiguracji może być wyzwaniem, zwłaszcza dla początkujących programistów. Dlatego warto bliżej przyjrzeć się dokumentacji języka TypeScript, gdzie znajdziemy szczegółowe informacje na temat instalacji, konfiguracji i pisania kodu.

Należy również pamiętać, że TypeScript jest rozszerzeniem języka JavaScript, więc można używać wszystkich bibliotek i frameworków dostępnych dla języka JavaScript. Warto również zapoznać się z narzędziami, takimi jak TypeScript Playground, które pomogą nam w szybkim testowaniu kodu oraz w lepszym poznaniu funkcjonalności języka.

## Zobacz również:

- [Oficjalna strona języka TypeScript](https://www.typescriptlang.org/)
- [Dokumentacja języka TypeScript](https://www.typescriptlang.org/docs/)
- [TypeScript Playground](https://www.typescriptlang.org/play)