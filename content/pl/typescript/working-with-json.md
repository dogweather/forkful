---
title:                "Praca z formatem json"
html_title:           "TypeScript: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz pracować z danymi w formacie JSON, TypeScript jest idealnym narzędziem dla Ciebie. Pozwala on na łatwą manipulację tym formatem danych, dzięki czemu możesz szybko i sprawnie przetwarzać różnego rodzaju informacje.

## Jak to zrobić

### Przygotowanie danych JSON

Aby rozpocząć pracę z danymi w formacie JSON, musisz najpierw przygotować odpowiednią strukturę danych. W przypadku TypeScript, wystarczy zadeklarować zmienną typu `object` i przypisać do niej odpowiednie wartości. 

```TypeScript
let user = {
    name: "Jan",
    age: 25,
    location: "Warszawa"
};
```

### Zapis i odczyt danych

Aby zapisać dane w formacie JSON, warto skorzystać z wbudowanej metody `JSON.stringify()`. Przykładowo, możesz zapisać dane użytkownika z poprzedniego przykładu w formie ciągu znaków.

```TypeScript
let userJSON = JSON.stringify(user);
console.log(userJSON);
// wynik: {"name":"Jan","age":25,"location":"Warszawa"}
```

Aby odczytać dane z formatu JSON, możesz użyć metody `JSON.parse()`, która zamienia ciąg znaków z powrotem na obiekt.

```TypeScript
let parsedUser = JSON.parse(userJSON);
console.log(parsedUser.name);
// wynik: Jan
```

### Praca z zagnieżdżonymi danymi

JSON pozwala na przechowywanie zagnieżdżonych danych, czyli obiektów lub tablic wewnątrz innych obiektów lub tablic. W przypadku TypeScript, możesz odwoływać się do nich po prostu przez kolejne właściwości lub indeksy.

```TypeScript
let product = {
    name: "Mydło",
    description: "Naturalne mydło o zapachu eukaliptusa",
    category: {
        name: "Kosmetyki",
        subcategory: "Higiena"
    }
};

console.log(product.category.name);
// wynik: Kosmetyki
console.log(product.category.subcategory);
// wynik: Higiena
```

## Głębsza analiza

### Typowanie danych JSON w TypeScript

Jedną z zalet korzystania z TypeScript jest możliwość definiowania typów danych. Dzięki temu, możesz mieć pewność, że dane, które przetwarzasz są zgodne z tym, co oczekujesz.

```TypeScript
interface User {
    name: string,
    age: number,
    location: string
};

let user: User = {
    name: "Jan",
    age: 25,
    location: "Warszawa"
};
```

W przykładzie powyżej, zadeklarowaliśmy interfejs `User`, który określa strukturę danych dla zmiennej `user`. Dzięki temu, jeśli spróbujemy przypisać do niej wartość innego typu, TypeScript poinformuje nas o błędzie.

### Praca ze strukturalnymi typami danych

Jedną z najbardziej przydatnych funkcji TypeScript jest możliwość pracy z obiektami o nieznanej strukturze lub tablicami o różnej długości. Możemy to osiągnąć przez użycie tzw. strukturalnych typów danych.

```TypeScript
interface Post {
    title: string,
    content: string
};

let posts: Post[] = [
    {
        title: "Nowy film",
        content: "Zapraszamy na premierę naszego najnowszego filmu!"
    },
    {
        title: "Wakacyjna oferta",
        content: "Spędź wakacje w piękny krajobrazach przyrody!"
    }
];
```

Nasza tablica `posts` może teraz zawierać dowolną ilość elementów o strukturze zgodnej z interfejsem `Post`.

## Zobacz także

- [Oficjalna dokumentacja TypeScript](https://www.typescriptlang.org/docs/home.html)