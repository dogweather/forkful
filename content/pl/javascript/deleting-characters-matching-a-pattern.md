---
title:    "Javascript: Usuwanie znaków pasujących do wzorca."
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Dlaczego

Często w trakcie pisania swojego programu możesz napotkać sytuację, w której chcesz usunąć pewien wzorzec znaków z tekstu. Może to wynikać z potrzeby wyciągnięcia konkretnej informacji lub uporządkowania danych. W takim przypadku bardzo przydatne może okazać się usunięcie znaków pasujących do zadanej formuły. W tym wpisie pokażemy, jak to zrobić w języku Javascript.

##Jak to zrobić

Usuwanie znaków pasujących do wzorca jest możliwe dzięki użyciu wyrażeń regularnych. Przedstawimy kilka przykładowych scenariuszy, gdzie może okazać się przydatne ich użycie.

###Przykład 1:

Załóżmy, że mamy tekst zawierający różne informacje, a my chcemy pozbyć się z niego cyfr. Możemy to zrobić za pomocą metody `replace()` i wyrażenia regularnego znajdującego się w nawiasach kwadratowych. Poniżej mamy kod, który pokazuje tę operację:

```Javascript
const text = "To jest przykład123"
const newText = text.replace(/[0-9]/g, '');
console.log(newText);
```

Output:
`To jest przykład`

W tym przypadku wykorzystaliśmy wyrażenie `/[0-9]/g` aby znaleźć wszystkie cyfry w tekście i zastąpić je pustym znakiem.

###Przykład 2:

Możemy również wykorzystać wyrażenia regularne do usunięcia wybranych słów z tekstu. W poniższym przykładzie chcemy usunąć słowo `"nie"` z naszego tekstu.

```Javascript
const text = "To nie jest przykład"
const newText = text.replace(/nie /g, '');
console.log(newText);
```

Output:
`To jest przykład`

W ten sposób możemy łatwo usunąć wybrane słowo lub frazę z tekstu, wykorzystując wyrażenia regularne.

##Wnikliwe podejście

Wyrażenia regularne są bardzo potężnym narzędziem w programowaniu i warto poświęcić trochę więcej czasu, aby dokładnie poznać ich możliwości. Mogą one służyć nie tylko do usuwania znaków pasujących do określonego wzorca, ale również do wyszukiwania, zamiany, walidacji danych i wiele więcej. W internecie można znaleźć wiele przydatnych poradników i kursów, które pomogą Ci pogłębić swoją wiedzę na temat wyrażeń regularnych.

##Zobacz również

- [MDN Web Docs: Wprowadzenie do wyrażeń regularnych w Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Wprowadzenie_do_wyra%C5%BCe%C5%84_regularnych)
- [W3Schools: Wyrażenia regularne w Javascript](https://www.w3schools.com/js/js_regexp.asp)
- [Kurs Udemy: Wprowadzenie do wyrażeń regularnych](https://www.udemy.com/course/wstep-do-wyrazen-regularnych/)