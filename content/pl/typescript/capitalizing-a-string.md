---
title:    "TypeScript: Kapitalizacja ciągu znaków"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Witajcie, programiści! Czy zdarzyło Wam się kiedyś, że potrzebowaliście zmienić format tekstu i zastanawialiście się jak to zrobić w TypeScript? Jeden z najczęściej wykonywanych zadań to kapitalizacja tekstu, czyli zmiana wszystkich liter na wielkie.

## Jak to zrobić

W tym przykładzie pokażemy Wam, jak łatwo i szybko zmienić wielkość liter w ciągu znaków w TypeScript. Wykorzystamy prostą metodę dostępną w większości języków programowania. Sprawdźmy!

```TypeScript
let text = "cześć, jestem tekstem"
let uppercaseText = text.toUpperCase();
console.log(uppercaseText);
```

### Wynik:

```TypeScript 
CZEŚĆ, JESTEM TEKSTEM
```

Jak widać, wystarczy użyć funkcji `toUpperCase()` na zmiennej tekstowej i wszystkie litery zostaną zmienione na wielkie. Proste, prawda? Dzięki temu funkcji możemy też zmienić wielkość liter w całym zdaniu lub w tekście z wczytanej zmiennej.

## Głębsza analiza

Jeśli chcesz się dowiedzieć więcej o kapitalizacji tekstu w TypeScript, możesz skorzystać z dodatkowych opcji, takich jak `charAt()` i `substr()`, aby uzyskać więcej kontroli nad tekstem. Jest to szczególnie przydatne, gdy potrzebujemy zmieniać wielkość liter w zależności od określonego warunku.

```TypeScript
function capitalizeFirstLetter(text: string) {
    return text.charAt(0).toUpperCase() + text.substr(1);
}

let text = "cześć, jestem tekstem"

console.log(capitalizeFirstLetter(text));
```

### Wynik:

```TypeScript
Cześć, jestem tekstem
```

Funkcja `capitalizeFirstLetter()` stworzona w tym przykładzie zamienia tylko pierwszą literę na wielką, a pozostałe pozostają niezmienione. Dzięki temu możemy uzyskać bardziej precyzyjną kapitalizację i uniknąć ewentualnych błędów.

## Zobacz również

Teraz już wiesz, jak łatwo zmienić wielkość liter w TypeScript! Spróbuj tego samodzielnie w swoich projektach. Jeśli chcesz się dowiedzieć więcej o pracy z tekstem w TypeScript, oto kilka przydatnych linków:

- [Metody dostępne w typie String w TypeScript](https://www.typescriptlang.org/docs/handbook/2/strings.html)
- [Tutorial na temat manipulacji tekstem w TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
- [Oficjalna dokumentacja TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Funkcje dostępne w języku JavaScript do manipulacji tekstem](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String)

Dziękujemy za przeczytanie naszego artykułu! Mam nadzieję, że było ono pomocne. Do zobaczenia!
# Zobacz również