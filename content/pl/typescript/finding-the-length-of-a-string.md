---
title:    "TypeScript: Obliczanie długości ciągu znaków"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego powinieneś/chciałbyś szukać długości stringa? Może planujesz stworzyć aplikację, która będzie wyświetlać pewien tekst, ale musisz wiedzieć, ile znaków ten tekst zawiera? Albo może chcesz zaimplementować funkcję, która będzie liczyć słowa w danym zdaniu? W obu tych przypadkach znajomość długości stringa jest niezbędna.

## Jak To Zrobić

```TypeScript
let str: string = "To jest przykładowy string";
console.log(str.length);
```
Ten prosty kawałek kodu w TypeScript pozwala nam wyświetlić długość stringa "To jest przykładowy string", która wynosi 26. Do uzyskania długości stringa używamy "length" po kropce, tak samo jak przy tablicach.

Możemy także ustawić zmienną na daną długość stringa i wyświetlić ją w naszej aplikacji:

```TypeScript
let str: string = "Długość tego zdania to";
console.log(str.length + "znaki.");
```
Output: Długość tego zdania to 26 znaki.

## Deep Dive

Istnieją różne sposoby na liczenie długości stringa w TypeScript. Możemy użyć metody "length" jak w przykładzie powyżej lub możemy skorzystać z funkcji "string.length", która również zwraca długość stringa. 

Warto również pamiętać, że w TypeScript każdy znak jest traktowany jako osobny element w stringu, nawet jeśli jest to znak specjalny lub emoji. Dlatego też długość stringa może się różnić w zależności od ilości znaków.

## Zobacz też

- [Dokumentacja TypeScript na temat stringów](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN web docs - String length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Porównanie długości stringów w TypeScript](https://stackoverflow.com/questions/15499609/compare-length-of-two-strings-with-typescript)