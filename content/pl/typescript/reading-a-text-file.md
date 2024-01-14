---
title:    "TypeScript: Odczytywanie pliku tekstowego"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się przeglądać dokumenty tekstowe i szukać szczególnego fragmentu informacji? Czy często pracujesz z plikami tekstowymi w swoim projekcie? Jeśli tak, to warto poznać, jak w łatwy i skuteczny sposób można odczytać plik tekstowy w języku TypeScript. Ten artykuł pokaże Ci, jak wykorzystać ten język programowania do szybkiego i profesjonalnego odczytywania plików tekstowych.

## Jak to zrobić

### Przygotowanie

Zanim zaczniesz czytać plik tekstowy w TypeScript, musisz przygotować odpowiednie środowisko. Skorzystaj z edytora tekstu, takiego jak Visual Studio Code, aby napisać kod oraz zainstaluj najnowszą wersję TypeScript za pomocą Node Package Manager (NPM).

### Komponent FileSystem

Dzięki modułowi FileSystem w języku TypeScript masz dostęp do funkcji potrzebnych do odczytywania plików tekstowych. Musisz najpierw zaimportować ten moduł, aby móc go użyć w swoim kodzie. Możesz to zrobić za pomocą poniższych poleceń:

```
import * as fs from "fs";
```

### Odczytywanie pliku

Po zaimportowaniu modułu FileSystem możesz rozpocząć proces odczytywania pliku tekstowego. W tym celu utwórz zmienną z odpowiednią scieżką do pliku. Następnie wywołaj funkcję `readFileSync` i przekaż do niej utworzoną zmienną jako argument. Oto przykładowy kod:

```
let filePath: string = "./moj_plik.txt";
let fileContents: string = fs.readFileSync(filePath, "utf-8");
```

Funkcja `readFileSync` zwróci zawartość pliku w formie tekstu, którą możesz wyświetlić za pomocą funkcji `console.log`.

### Przykładowy wynik

Jeśli plik tekstowy zawiera następującą treść:

```
To jest mój plik tekstowy.
Zawiera on kilka linijek tekstu.
```

To w wyniku otrzymasz następujący output:

```
To jest mój plik tekstowy.
Zawiera on kilka linijek tekstu.
```

## Dogłębne studium

Język TypeScript oferuje wiele kolejnych funkcji, które możesz wykorzystać do odczytywania plików tekstowych. Możesz na przykład użyć funkcji `readFile` zamiast `readFileSync` w celu asynchronicznego odczytu pliku. Możesz także wykorzystać funkcje `readStream` lub `createReadStream` do odczytywania dużych plików tekstowych w sposób bardziej efektywny.

Powinieneś również pamiętać o poprawnym obsłużeniu błędów podczas odczytywania plików, na przykład błędów związanych z niepoprawną ścieżką lub brakiem dostępu do pliku. Możesz wykorzystać konstrukcję `try-catch` lub funkcję `on` dla obiektu `fs`.

## Zobacz również

Jeśli jesteś zainteresowany dalszym poznawaniem języka TypeScript, zobacz inne artykuły w naszej serii "Programowanie w TypeScript". Możesz także odwiedzić oficjalną dokumentację języka oraz skorzystać z dostępnych tutoriali.

- Dokumentacja języka TypeScript: https://www.typescriptlang.org/docs/
- Tutoriale o TypeScript: https://www.tutorialspoint.com/typescript/
- Programowanie w TypeScript: Działanie z plikami JSON: https://www.freecodecamp.org/news/typescript-json-tutorial/
- Poradnik dla początkujących w języku TypeScript: https://www.javacodegeeks.com/how-to-program-with-typescript