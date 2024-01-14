---
title:    "TypeScript: Odczytywanie pliku tekstowego"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś przeczytać plik tekstowy za pomocą TypeScript, ale nie wiedziałeś od czego zacząć? W tym wpisie dowiesz się, jak to zrobić w prosty sposób.

## Jak to zrobić

Aby odczytać plik tekstowy za pomocą TypeScript, musimy wykonać kilka kroków:

1. Na początek, musimy zaimportować moduł `fs` używając słowa kluczowego `import`.
2. Następnie, używając funkcji `readFileSync` z modułu `fs`, możemy odczytać zawartość pliku tekstowego i przypisać ją do zmiennej.
3. Aby wyświetlić zawartość pliku tekstowego, wystarczy użyć funkcji `console.log` i przekazać jako argument odczytaną zmienną.

```TypeScript
import * as fs from 'fs';

let fileContent = fs.readFileSync('plik.txt', 'utf-8');
console.log(fileContent);
```

Gdy wykonamy ten kod, w konsoli pojawi się zawartość pliku tekstowego `plik.txt`. Proste, prawda?

## Deep Dive

Jeśli chcesz się dokładniej przyjrzeć jak działa odczytywanie pliku tekstowego za pomocą TypeScript, dowiedz się więcej o funkcji `readFileSync` i jej możliwościach. Oprócz podstawowej funkcjonalności, możesz również zdefiniować opcjonalne parametry, takie jak kodowanie tekstu czy tryb odczytu pliku.

## Zobacz również

Sprawdź te przydatne linki, aby dowiedzieć się więcej o używaniu TypeScript:

- [Dokumentacja TypeScript](https://www.typescriptlang.org/docs)
- [Kurs TypeScript dla początkujących](https://www.udemy.com/course/typescript-kurs/)
- [Kurs TypeScript w praktyce](https://www.udemy.com/course/typescript-w-praktyce/)