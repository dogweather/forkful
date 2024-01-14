---
title:                "TypeScript: Rozpoczynając nowy projekt"
programming_language: "TypeScript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Rozpoczęcie nowego projektu to wyjątkowo fascynujące przedsięwzięcie. Poza wymaganą wiedzą i umiejętnościami, budowa czegoś od podstaw daje niepowtarzalną satysfakcję i możliwość kreatywności. Jeśli masz pomysł na nowy projekt, którego realizacja wymaga użycia języka TypeScript, ten artykuł jest dla Ciebie!

## Jak to zrobić

Pierwszym krokiem do rozpoczęcia nowego projektu w języku TypeScript jest pobranie i zainstalowanie odpowiedniego środowiska. Zalecamy korzystanie z popularnego narzędzia - npm (Node Package Manager), które pozwala na zarządzanie pakietami w projekcie.

Po zainstalowaniu npm, możesz przejść do tworzenia pliku z kodem TypeScript. Stwórzmy na przykład proste funkcje, które będą dodawać dwa podane przez użytkownika parametry i zwracać ich sumę:

```TypeScript
function suma(a: number, b: number) {
  return a + b;
}

const liczba1: number = 5;
const liczba2: number = 10;

console.log(suma(liczba1, liczba2));
// zwróci 15
```

W powyższym przykładzie użyliśmy typów zmiennych - `number` - co pozwala na bardziej precyzyjne określenie typów danych używanych w funkcji. W przypadku niewłaściwych typów argumentów, TypeScript wyświetli błąd kompilacji.

Możemy również tworzyć klasy i interfejsy w naszych projektach TypeScript. Klasa to zbiór zmiennych i funkcji (metod), podczas gdy interfejs określa strukturę i typy danych, które muszą być implementowane przez klasę. Przykład:

```TypeScript
interface Figura {
  nazwa: string;
  pole: number;
  obwod: number;
}

class Kwadrat implements Figura {
  nazwa = 'Kwadrat';
  pole: number;
  obwod: number;

  constructor(public bok: number) {
    this.pole = bok * bok;
    this.obwod = 4 * bok;
  }
}

const nowyKwadrat = new Kwadrat(5);

console.log(nowyKwadrat.nazwa);
// zwróci 'Kwadrat'
console.log(nowyKwadrat.pole);
// zwróci 25
```

W przypadku utworzenia nowego obiektu klasy `Kwadrat`, musimy przekazać wartość dla zmiennej `bok` - inaczej TypeScript wyświetli błąd kompilacji.

## Głębsza analiza

Rozpoczęcie nowego projektu w języku TypeScript może być trochę przytłaczające dla osób początkujących, ze względu na dużą ilość dostępnej dokumentacji oraz różnorodność funkcji i możliwości języka. Dlatego ważne jest, aby znaleźć odpowiednie źródła informacji i korzystać z nich regularnie. Początkowe trudności z pewnością zrekompensują nam późniejsze korzyści z użytkowania języka TypeScript w naszych projektach.

## Zobacz też

- https://www.typescriptlang.org/ - oficjalna strona TypeScript
- https://www.npmjs.com/ - strona npm (Node Package Manager)
- https://www.w3schools.com/typescript/ - interaktywny przewodnik po TypeScript na stronie W3Schools