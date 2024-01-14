---
title:                "Arduino: Wyciąganie podciągów"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Z pewnością wielu z nas miało już taki problem - musimy przetworzyć pewien tekst, ale interesują nas tylko pewne jego fragmenty. Albo chcemy odseparować część tekstu od reszty i wykorzystać ją do innego celu. W takich sytuacjach bardzo przydatne jest umiejętne wydobycie podciągów - czyli fragmentów tekstu - z płytki Arduino. Dzięki temu możemy dokonać dokładnej analizy, porównania lub wykorzystania tylko wybranych informacji w naszym projekcie.

## Jak to zrobić

Aby wydobyć podciągi z tekstu na płytki Arduino, wystarczy wykorzystać kilka prostych funkcji. W pierwszej kolejności musimy wykorzystać funkcję `substring()`, która zwraca tylko określony fragment tekstu. Należy podać jej dwa parametry: pierwszy to indeks, od którego chcemy rozpocząć wydobycie podciągu, a drugi to indeks końcowy, gdzie chcemy zakończyć. Aby wyświetlić wydobrany podciąg, możemy wykorzystać funkcję `Serial.println()`.

```Arduino
// Przykład 1
String tekst = "To jest przykładowy tekst.";
String podciag = tekst.substring(3, 10); // wynikiem będzie "jest pr"
Serial.println(podciag); // wyświetli tylko "jest pr"
```

Kolejną przydatną funkcją jest `indexOf()`, która pozwala znaleźć indeks pierwszego wystąpienia danego znaku lub ciągu znaków. Dzięki temu możemy łatwo znaleźć początek danego wyrazu lub frazy w tekście.

```Arduino
// Przykład 2
String data = "01.01.2020";
int indeks = data.indexOf("."); // indeks będzie równy 2
int dzien = data.substring(0, indeks).toInt(); // wydobycie dnia jako liczby całkowitej
Serial.println(dzien); // wyświetli 1
```

Możemy również wykorzystać funkcję `lastIndexOf()` do znalezienia indeksu ostatniego wystąpienia określonego znaku lub ciągu znaków w tekście. To, w połączeniu z `substring()`, pozwala na odwrócenie tekstu lub przejście do końca danego wyrazu lub frazy.

```Arduino
// Przykład 3
String imie = "Jan Kowalski";
int indeks = imie.lastIndexOf(" "); // indeks będzie równy 3
String nazwisko = imie.substring(indeks + 1); // wydobycie tylko nazwiska
Serial.println(nazwisko); // wyświetli "Kowalski"
```

## Głębszy zanurzenie

W praktyce, dokonując wydobycia podciągów z tekstu, musimy odporność na różne błędy i wyjątki. Na przykład, gdy nie możemy znaleźć określonego znaku lub podciągu w tekście, funkcja `substring()` zwróci pustą wartość, a `indexOf()` lub `lastIndexOf()` zwrócą wartość -1. Dlatego warto zawsze sprawdzać, czy otrzymane wartości są poprawne.

Innym ważnym aspektem jest wydajność. Wykorzystywanie wielu funkcji `substring()`, `indexOf()`, `lastIndexOf()` itp. w pętli lub w dużym tekście może spowolnić działanie naszego projektu. W takim wypadku lepiej byłoby użyć bardziej zaawansowanych algorytmów, takich jak wykorzystanie wskaźników lub wyrażeń regularnych.

## Zobacz również

- [Dokumentacja języka Arduino dotycząca funkcji String](https://www.arduino.cc/reference/en/language/variables/data