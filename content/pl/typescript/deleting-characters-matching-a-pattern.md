---
title:    "TypeScript: Usuwanie znaków pasujących do wzorca"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Wykasowywanie znaków pasujących do wzoru jest jedną z przydatnych funkcji w TypeScript, która może pomóc w pracy z tekstem. Pozwala ona na usunięcie niepożądanych znaków lub ciągów znaków z tekstu, co może być szczególnie przydatne w przypadku przetwarzania danych lub walidacji użytkowników.

## Jak To Zrobić

Wykasowywanie znaków pasujących do wzoru w TypeScript jest bardzo proste i można to zrobić w sposób następujący:

```TypeScript
let tekst = "To jest przykładowy tekst!";
let wzorzec = /[a-z]/g; // znaki małych liter  
let wynik = tekst.replace(wzorzec, ''); // usunięcie znaków pasujących do wzoru 
console.log(wynik); // wynik: T        
```

Kod ten używa funkcji `replace()` do usunięcia wszystkich małych liter ze zmiennej `tekst`. Wcześniej jednak zdefiniowany został wzorzec z użyciem wyrażenia regularnego `[a-z]`, które oznacza każdy znak z zakresu `a-z`. Dzięki temu, funkcja `replace()` wie, jakie znaki należy usunąć z podanego tekstu.

Można również wykorzystać to narzędzie do usuwania innych znaków, na przykład cyfr czy nawet całych wyrazów. Przykładowo, wyrażenie regularne `/[0-9]/g` będzie usuwać wszystkie cyfry z tekstu, a wyrażenie regularne `/\bprzykładowy\b/g` będzie usuwać wszystkie wystąpienia wyrazu "przykładowy" z tekstu.

Wynikiem działania funkcji `replace()` jest nowa zmienna, zawierająca tekst po usunięciu odpowiednich znaków. Jest to bardzo przydatne przy przetwarzaniu danych lub tworzeniu funkcji walidacyjnych, które wymagają określonych danych, wolnych od niepożądanych znaków.

## Głębszy Wgląd

Wykasowywanie znaków pasujących do wzoru może odbywać się na kilka różnych sposobów, w zależności od potrzeb danego projektu. Oprócz wykorzystania wyrażeń regularnych, można również użyć funkcji `split()` w połączeniu z `join()`, aby usunąć znaki pasujące do wzoru i złączyć tekst ponownie.

Warto również zauważyć, że funkcja `replace()` przyjmuje dwa parametry - pierwszy to wzorzec, a drugi to zmienna, którą chcemy wstawić zamiast znaków pasujących do wzoru. Możemy więc użyć tej funkcji nie tylko do usuwania znaków, ale także do ich zamiany na inne, dowolne wartości.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych i ich zastosowaniach w TypeScript, polecamy zapoznać się z poniższymi źródłami:

- [Pisanie i testowanie wyrażeń regularnych w TypeScript](https://blog.bitsrc.io/writing-and-testing-regular-expressions-in-typescript-132a7f6dd51a)
- [Dokumentacja wyrażeń regularnych w TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)