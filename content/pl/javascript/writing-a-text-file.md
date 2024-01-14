---
title:    "Javascript: Tworzenie pliku tekstowego"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego jest ważnym elementem dla wielu programistów Javascript. Jest to niezbędne w wielu przypadkach, takich jak zapisywanie danych, konfiguracji lub wyników działania programu. Dzięki pisaniu pliku tekstowego, możemy zachować ważne informacje i korzystać z nich w przyszłości.

## Jak to zrobić

Aby napisać plik tekstowy w Javascript, musimy użyć wbudowanego modułu `fs` (file system). Najpierw musimy utworzyć nowy plik przy użyciu metody `writeFile()`, która przyjmuje trzy parametry - nazwę pliku, dane do zapisania oraz funkcję zwrotną, która zostanie uruchomiona po zapisaniu pliku.

Poniżej znajduje się przykładowy kod w Javascript, który tworzy plik tekstowy o nazwie "dane.txt" i zapisuje w nim informacje o imieniu i wieku osoby. 

```Javascript
const fs = require('fs');

const dane = "Imię: John, Wiek: 25";

fs.writeFile('dane.txt', dane, function (err) {
    if (err) throw err;
    console.log('Plik został zapisany!');
});
```

Po uruchomieniu tego kodu, w naszym folderze pojawi się plik tekstowy o nazwie "dane.txt" z zawartością "Imię: John, Wiek: 25".

## Głębszy zanurzenie

Ważne jest, aby wiedzieć, że przy użyciu metody `writeFile()` zapisujemy całkowicie nowy plik. Jeśli chcemy dopisywać dane do istniejącego pliku, powinniśmy użyć metody `appendFile()`.

Dodatkowo, możemy określić format naszego pliku tekstowego, dodając odpowiednią flagę jako trzeci parametr. Na przykład, jeśli chcemy zapisać nasze dane w formacie JSON, użyjemy flagi `{ flag: 'a', encoding: 'utf8' }`.

## Zobacz również

- [Dokumentacja modułu fs w języku Javascript](https://nodejs.org/api/fs.html)
- [Przykładowy kod pisania pliku tekstowego w Javascript](https://www.geeksforgeeks.org/writing-data-to-a-text-file-using-javascript/)
- [Przykładowy kod dopisywania danych do istniejącego pliku tekstowego w Javascript](https://www.guru99.com/reading-and-writing-files-in-nodejs.html#2)