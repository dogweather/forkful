---
title:    "TypeScript: Sprawdzanie istnienia katalogu"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego sprawdzanie czy istnieje katalog?

Sprawdzanie, czy katalog istnieje jest ważnym krokiem w procesie programowania, ponieważ pozwala na potwierdzenie, czy dany katalog istnieje, zanim podejmiemy dalsze działania, takie jak jego odczyt lub zapis. Nawiązując do języka JavaScript, TypeScript jest językiem programowania wysokiego poziomu, który zapewnia statyczną typizację, czyli sprawdzanie typów zmiennych podczas kompilacji kodu. Dlatego jest to szczególnie ważne dla programistów, którzy chcą uniknąć błędów w czasie wykonania.

## Jak to zrobić?

Sprawdzenie czy istnieje katalog jest możliwe dzięki użyciu funkcji `existsSync` z modułu `fs`. Poniższy przykład kodu w TypeScript pokazuje, jak można sprawdzić istnienie katalogu o nazwie "dane" w bieżącym katalogu.

```TypeScript
import { existsSync } from "fs";

if (existsSync("dane")) {
  console.log("Katalog istnieje.");
}
else {
  console.log("Katalog nie istnieje.");
}
```

W tym przykładzie używamy operatora `if` do sprawdzenia, czy funkcja `existsSync` zwraca wartość `true`, co oznacza, że katalog istnieje. W przeciwnym razie, jeśli funkcja zwraca `false`, wyświetlana jest informacja, że katalog nie istnieje.

## Głębszy zanurzenie

Funkcja `existsSync` pochodzi z modułu `fs`, który dostarcza narzędzia do obsługi systemu plików w języku Node.js. Jest to funkcja synchroniczna, co oznacza, że jej wykonanie zatrzyma wykonywanie kodu do momentu zakończenia operacji sprawdzania. Inną opcją jest użycie funkcji `statSync`, która zwraca obiekt z informacjami o danym pliku lub katalogu, w tym informacje o istnieniu.

W przypadku, gdy chcemy nie tylko sprawdzić istnienie katalogu, ale także jego zawartość, możemy zastosować pętlę `forEach` i funkcję `readdirSync` z modułu `fs`. Funkcja `readdirSync` zwraca tablicę zawierającą nazwy wszystkich plików i katalogów w danym katalogu, a pętla `forEach` pozwala na wykonywanie określonych działań dla każdego elementu w tej tablicy.

```TypeScript
import { readdirSync } from "fs";

readdirSync("dane").forEach(file => {
  console.log(file); // Wyświetla nazwy plików i katalogów w katalogu "dane"
});
```

## Zobacz również

- [Dokumentacja funkcji existsSync w języku TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html)
- [Moduł fs w języku Node.js](https://nodejs.org/api/fs.html)
- [Poradnik dla początkujących w języku TypeScript](https://www.digitalocean.com/community/tutorials/typescript-basics-concepts-an-introduction-for-beginners)