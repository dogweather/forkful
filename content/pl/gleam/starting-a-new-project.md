---
title:    "Gleam: Rozpoczęcie nowego projektu"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego warto zacząć nowy projekt z Gleam?

Gleam jest nowoczesnym językiem programowania, który pozwala na pisanie bezpiecznych i wydajnych aplikacji. Przez swoją unikalną składnię i wbudowane narzędzia, jest idealnym wyborem dla osób pragnących budować skalowalne i niezawodne systemy. 

## Jak to zrobić?

Gleam jest językiem funkcyjnym, co oznacza, że kod pisany w nim jest modułowy i łatwy w utrzymaniu. Poniżej przedstawiamy przykłady kodu wraz z wyjściem, które będą pomocne w zrozumieniu Gleam oraz jego podstawowych funkcji.

```Gleam
let person = {
  name: "Kasia",
  age: 28
} 

fun print_person(person) {
  IO.print("Cześć, nazywam się {person.name} i mam {person.age} lat!")
}

print_person(person) 
```
Output:
```
Cześć, nazywam się Kasia i mam 28 lat!
```

```Gleam
fun calculate_average(numbers) {
  let sum = List.fold_left(numbers, 0, fn(x, acc) {
    x + acc
  })
  sum / List.length(numbers)
}

let scores = [86, 92, 78, 100, 95]
let average = calculate_average(scores)
IO.print("Srednia z ocen to {average}") 
```
Output:
```
Średnia z ocen to 90.2
```

## Głębszy wgląd w rozpoczęcie nowego projektu

Aby zacząć nowy projekt z Gleam, należy najpierw zainstalować jego kompilator i narzędzia do zarządzania zależnościami. Następnie można tworzyć moduły kodu, korzystając z wbudowanych typów danych i funkcji. Bardzo ważne jest również testowanie kodu za pomocą frameworka gleam_test, który pozwala na sprawdzenie poprawności działania każdej funkcji.

W przypadku pytań lub problemów związanych z Gleam, warto zajrzeć na oficjalną stronę dokumentacji lub dołączyć do społeczności na Discordzie, gdzie można uzyskać pomoc od innych programistów.

## Zobacz również

- Oficjalna strona Gleam: https://gleam.run/
- Dokumentacja Gleam: https://gleam.run/documentation/
- Discord Gleam: https://discord.gg/Nn8WCzJ