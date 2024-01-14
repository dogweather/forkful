---
title:    "Javascript: Wyodrębnianie podłańcuchów"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Często w trakcie programowania potrzebujemy wyodrębnić pewną część tekstu z większego ciągu znaków. Możemy chcieć np. pobrać tylko imię z wprowadzonego przez użytkownika pełnego imienia i nazwiska. W takich przypadkach bardzo przydatne jest wykorzystanie funkcji pozwalających na wyciągnięcie podciągu znaków, czyli tzw. substrings.

## Jak to zrobić

W Javascripcie do wyciągania substrings możemy użyć metody `substring()` lub `substr()`. Są one bardzo podobne, jednak mają nieco inny sposób działania. W obu przypadkach podajemy jako parametry indeks początkowy i końcowy. Indeks początkowy określa, od którego znaku zaczynamy wybierać podciąg, a indeks końcowy - na którym znaku kończymy. Przykładowo, jeśli chcemy wyodrębnić z ciągu "John Smith" tylko imię, podamy indeks początkowy jako 0 (pierwszy znak w ciągu) i indeks końcowy jako 4 (pierwsze cztery znaki - "John").

```Javascript
let fullName = "John Smith";
let firstName = fullName.substring(0, 4);
console.log(firstName); // wynik: John
```

W przypadku metody `substr()` możemy także podać tylko jeden parametr - indeks początkowy - oraz liczbę znaków, które chcemy wyodrębnić. Przykładowo, jeśli chcemy wyodrębnić z ciągu "John Smith" tylko nazwisko, podamy indeks początkowy jako 5 (pozycja pierwszej litery nazwiska) oraz liczbę znaków jako 5 (5 znaków - "Smith").

```Javascript
let fullName = "John Smith";
let lastName = fullName.substr(5, 5);
console.log(lastName); // wynik: Smith
```

## Wkład w głąb

Funkcje `substring()` i `substr()` działają w podobny sposób, ale mają pewne różnice, które warto znać. Przede wszystkim, metoda `substr()` może także przyjmować liczbę ujemną jako drugi parametr, co oznacza liczbę znaków od końca ciągu. Przykładowo, jeśli chcemy wyodrębnić ostatnie 3 litery z nazwiska "Smith", możemy podać indeks początkowy jako -3 (3 od końca) oraz liczbę znaków jako 3.

```Javascript
let lastName = "Smith";
let lastLetters = lastName.substr(-3, 3);
console.log(lastLetters); // wynik: ith
```

Dodatkowo, metoda `substr()` pozwala także na podanie tylko jednego parametru - indeksu początkowego - wtedy wyodrębniony zostanie cały podciąg od tego znaku do końca ciągu.

```Javascript
let fullName = "John Smith";
let lastPart = fullName.substr(5);
console.log(lastPart); // wynik: Smith
```

## Zobacz również
- [Metoda substring() w dokumentacji MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/substring)
- [Metoda substr() w dokumentacji MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/substr)