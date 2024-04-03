---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:21.880946-07:00
description: "Jak to zrobi\u0107: Tworzenie i u\u017Cywanie asocjacyjnych tablic w\
  \ TypeScript jest proste. Oto kr\xF3tki przewodnik."
lastmod: '2024-03-13T22:44:35.131607-06:00'
model: gpt-4-0125-preview
summary: "Tworzenie i u\u017Cywanie asocjacyjnych tablic w TypeScript jest proste."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak to zrobić:
Tworzenie i używanie asocjacyjnych tablic w TypeScript jest proste. Oto krótki przewodnik:

```TypeScript
// Deklarowanie asocjacyjnej tablicy
let user: { [key: string]: string } = {};

// Dodawanie danych
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

Wynik:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

Iterowanie przez pary klucz-wartość jest również łatwe:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Wynik:

```TypeScript
name: Jane Doe
email: jane@example.com
```

A jeśli masz do czynienia z mieszanką typów danych, system typów TypeScript przychodzi z pomocą:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Wynik:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## W głąb
W TypeScript, to co nazywamy asocjacyjnymi tablicami, są w zasadzie obiektami. Historycznie, w językach takich jak PHP, asocjacyjne tablice są fundamentalnym typem, ale JavaScript (i przez rozszerzenie, TypeScript) używa obiektów do tego celu. To podejście jest zarówno siłą, jak i ograniczeniem. Obiekty zapewniają wysoce dynamiczną strukturę do kojarzenia ciągów znaków z wartościami, ale nie są przeznaczone do użytku jako "tablice" w tradycyjnym sensie. Na przykład, nie można bezpośrednio używać na tych obiektach metod tablicowych takich jak `push` czy `pop`.

W przypadkach, gdy potrzebujesz uporządkowanych kolekcji par klucz-wartość z operacjami podobnymi do tablic, TypeScript (i nowoczesny JavaScript) oferuje obiekt `Map`:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Chociaż system typów TypeScript i funkcje ES6, takie jak `Map`, oferują potężne alternatywy, zrozumienie, jak używać obiektów jako asocjacyjnych tablic jest przydatne w scenariuszach, gdy literały obiektów są bardziej wydajne lub przy pracy z strukturami danych JSON. To wszystko sprowadza się do wyboru właściwego narzędzia do zadania.
