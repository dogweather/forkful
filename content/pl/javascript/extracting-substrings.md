---
title:                "Javascript: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Wyciąganie podciągów jest jedną z podstawowych umiejętności w programowaniu Javascript. Pozwala nam na manipulowanie tekstami i wyodrębnianie z nich potrzebnych nam informacji. To też często jedna z pierwszych zagadnień, które pojawiają się na drodze każdego młodego programisty. Dlatego warto się z tym tematem zapoznać i nauczyć się go stosować.

## Jak to zrobić? 

```Javascript
const tekst = "To jest przykładowy tekst.";
const podciag = tekst.substring(3,9);
console.log(podciag);
```

W powyższym przykładzie wykorzystujemy funkcję `substring()`, która ma dwa parametry - indeks początkowy i indeks końcowy. W wyniku otrzymujemy podciąg, który zaczyna się od 3 znaku i kończy na 9. W naszym przypadku będzie to "jest".

Dodatkowo, funkcja `substring()` jest wyjątkowo użyteczna w połączeniu z metodą `indexOf()`, która pozwala nam na znalezienie indeksu danego znaku lub słowa w tekście. Dzięki temu możemy precyzyjniej wybrać interesujący nas podciąg.

```Javascript
const tekst = "To jest przykładowy tekst.";
const index = tekst.indexOf("przykładowy"); //zwraca wartość 7
const podciag = tekst.substring(index, tekst.length);
console.log(podciag);
```

W efekcie otrzymamy podciąg, który zaczyna się od słowa "przykładowy" i kończy się na końcu tekstu - w naszym przypadku będzie to całe zdanie "przykładowy tekst".

## Głębszy wgląd

Podczas pracy z funkcją `substring()`, warto pamiętać o różnicach między indeksami liczb a indeksami znaków. W przypadku indeksów liczb, pierwszy znak ma indeks 0, a ostatni n-1 (gdzie n to długość tekstu). Natomiast w przypadku indeksów znaków, pierwszy znak jest oznaczany jako 1, a ostatni jako n.

Kolejną przydatną funkcją jest `substr()` - działa podobnie jak `substring()`, ale przyjmuje tylko dwa parametry - indeks początkowy i długość podciągu.

W przypadku pracy z dłuższymi tekstami, warto również zwrócić uwagę na różnice między metodą `substring()` a `slice()`. Ta druga działa w podobny sposób, ale pozwala na podanie indeksów ujemnych, co ułatwia wybieranie podciągów od końca tekstu.

## Zobacz również

Jeśli chcesz pogłębić swoją wiedzę na temat manipulowania tekstami w Javascript, warto zapoznać się z tymi artykułami:

- [Manipulowanie tekstami w Javascript](https://javascript.info/string)
- [Metody pracy z tekstem w Javascript](https://www.w3schools.com/js/js_string_methods.asp)

Pamiętaj, że jedyną skuteczną metodą nauki jest praktyka, więc nie wahaj się tworzyć własnych przykładów i eksperymentować z funkcjami `substring()`, `substr()` i `slice()`. Powodzenia!