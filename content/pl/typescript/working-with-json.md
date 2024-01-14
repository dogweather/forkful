---
title:                "TypeScript: Praca z jsonem"
simple_title:         "Praca z jsonem"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON jest jednym z najczęściej używanych formatów danych w programowaniu. Jest on dobrym wyborem, gdy chcesz przesyłać, przechowywać lub odczytywać dane w łatwy i wygodny sposób. W tym artykule dowiesz się, dlaczego warto pracować z tym formatem oraz jak to zrobić.

## Jak to zrobić

Pierwszym krokiem jest zainstalowanie TypeScript, jeśli jeszcze go nie posiadasz. Następnie, stwórz nowy plik **.ts** i dołącz bibliotekę JSON, aby móc pracować z tym formatem. W przykładach poniżej zostały wykorzystane proste obiekty, ale możesz również pracować złożonymi strukturami, takimi jak tablice i obiekty w obiekcie.

```TypeScript
import * as data from "./sample.json";

console.log(data);
```
Na wyjściu otrzymamy cały obiekt JSON, który znajduje się w pliku **sample.json**. Aby uzyskać dostęp do konkretnych wartości, możemy wykorzystać notację kropkową lub nawiasową.

```TypeScript
console.log(data.name); // John
console.log(data["age"]); // 25
```
Możemy także użyć pętli, aby przeiterować przez wszystkie wartości obiektu.

```TypeScript
for (let key in data) {
  console.log(key + ": " + data[key]);
}
```
Oczywiście, istnieje wiele innych sposobów na manipulowanie i wyświetlanie danych z obiektu JSON w TypeScript. To tylko kilka podstawowych przykładów, aby pomóc Ci zacząć pracę z tym formatem.

## Głębsza analiza

Ważnym aspektem pracy z JSON-em jest rozumienie jego struktury. Wiele bibliotek oferuje wiele metod do manipulacji danymi, ale jeśli nie znasz dokładnie, jak twój obiekt JSON jest zbudowany, może być trudno znaleźć potrzebne informacje.

Zaleca się również przestrzeganie konwencji nazewniczych podczas tworzenia i przechowywania obiektów JSON. Na przykład, używanie słów kluczowych jako nazw własności może prowadzić do błędów, ponieważ są one zarezerwowane przez język.

## Zobacz też

- Dokumentacja TypeScript: https://www.typescriptlang.org/docs/

- Dokumentacja JSON: https://www.json.org/json-pl.html

- Biblioteka JSON dla TypeScript: https://github.com/douglascrockford/JSON-js