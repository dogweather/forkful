---
title:                "PHP: Usuwanie znaków pasujących do wzorca"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Niektóre problemy programistyczne mogą wymagać usunięcia określonych znaków z ciągu tekstowego zgodnie z określonym wzorcem. To szczególnie ważne w przypadku analizy danych lub filtrowania informacji. W tym artykule dowiesz się, jak skutecznie i prosto usuwać znaki zgodnie z określonym wzorcem w języku PHP.

## Jak to zrobić

Aby usunąć znaki zgodnie z określonym wzorcem, musimy skorzystać z funkcji PHP `preg_replace()`. Przyjmuje ona trzy argumenty: wzorzec do wyszukania, tekst do zastąpienia oraz ciąg tekstowy, w którym chcemy dokonać zmian. Poniżej przedstawione jest prosty przykład kodu:

```PHP
$text = "Example string with unwanted characters";
$clean_text = preg_replace("/[^a-zA-Z0-9]/", "", $text);
echo $clean_text; // wypisze "Examplestringwithunwantedcharacters"
```

W powyższym przykładzie, za pomocą wyrażenia regularnego `/[^a-zA-Z0-9]/` usuwamy wszystkie znaki z wyjątkiem liter i cyfr. Możesz dostosować wzorzec według swoich potrzeb, aby usuwać konkretne znaki lub sekwencje znaków.

Możesz również użyć kilku wyrażeń regularnych jednocześnie, aby usunąć różne wzorce znaków. Poniżej znajduje się przykładowy kod:

```PHP
$text = "Example string with unwanted characters";
$clean_text = preg_replace(["/[^a-z]/", "/[0-9]/"], "", $text);
echo $clean_text; // wypisze "Examplestringwithunwantedcharacters"
```

W tym przypadku, używamy dwóch wyrażeń regularnych, aby usunąć wszystkie duże litery oraz cyfry.

## Deep Dive

Korzystanie z funkcji `preg_replace()` daje nam dużą elastyczność w usuwaniu znaków zgodnie z określonym wzorcem. Możemy na przykład wykorzystać znaki specjalne, aby dokładniej dopasować wzorzec. Oto kilka najczęściej używanych znaków specjalnych wraz z ich znaczeniem:

- `.` - dopasowuje dowolny pojedynczy znak (poza nową linią),
- `*` - dopasowuje 0 lub więcej wystąpień poprzedniego znaku,
- `+` - dopasowuje 1 lub więcej wystąpień poprzedniego znaku,
- `?` - dopasowuje 0 lub 1 wystąpienie poprzedniego znaku,
- `{n}` - dopasowuje dokładnie n wystąpień poprzedniego znaku,
- `{n,}` - dopasowuje n lub więcej wystąpień poprzedniego znaku,
- `{n,m}` - dopasowuje od n do m wystąpień poprzedniego znaku.

Najlepszym sposobem na zrozumienie sposobu działania wyrażeń regularnych jest po prostu je wypróbować i eksperymentować z różnymi kombinacjami.

## Zobacz także

- [Dokumentacja funkcji `preg_replace()` w języku PHP](https://www.php.net/manual/en/function.preg-replace.php)
- [Tutorial o wyrażeniach regularnych w języku PHP](https://www.tutorialrepublic.com/php-tutorial/php-regular-expressions.php)
- [Darmowe narzędzie do testowania wyrażeń regularnych](https://regex101.com/)