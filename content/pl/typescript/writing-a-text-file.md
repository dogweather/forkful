---
title:                "TypeScript: Tworzenie pliku tekstowego"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego tworzenie plików tekstowych jest tak ważną częścią programowania? Pisanie plików tekstowych może być nieco nużące, ale jest to niezbędna umiejętność dla każdego programisty. Pliki tekstowe pozwalają na zapisywanie danych w czytelnej formie, dzięki czemu można je łatwo przeczytać i analizować. W tym artykule dowiesz się, dlaczego pisanie plików tekstowych jest ważne i jak można to zrobić w języku TypeScript.

## Jak to zrobić

Pisanie plików tekstowych w TypeScript jest bardzo prostym procesem, który wymaga użycia wbudowanego modułu 'fs'. Aby rozpocząć, należy utworzyć nowy plik tekstowy o nazwie 'tekst.txt' za pomocą metody 'createWriteStream'. Następnie należy ustawić kodowanie pliku na 'utf-8' za pomocą metody 'setEncoding'.

```TypeScript
import * as fs from 'fs';

// tworzenie nowego pliku tekstowego
const plik = fs.createWriteStream('tekst.txt');

// ustalenie kodowania pliku na utf-8
plik.setEncoding('utf-8');
```

Następnie można użyć metody 'write' do zapisania danych w pliku tekstowym. Przykładowe dane do zapisania to "Witaj, świecie!".

```TypeScript
// zapisywanie danych w pliku
plik.write("Witaj, świecie!");

// zamykanie pliku
plik.end();
```

Po wykonaniu powyższych kroków, w bieżącym folderze pojawi się nowy plik tekstowy o nazwie 'tekst.txt' z zapisanym tekstem "Witaj, świecie!". Proces ten można powtarzać, aby dopisywać kolejne dane do pliku.

## Deep Dive

Pisanie plików tekstowych w języku TypeScript nie ogranicza się tylko do zapisywania tekstu. Można również wykorzystać wbudowane moduły, aby tworzyć bardziej złożone pliki. Na przykład można użyć modułu 'path' do tworzenia nowych folderów lub modułu 'fs/promises' do asynchronicznego zapisywania danych w pliku.

Pamiętaj również, aby zawsze zamknąć plik po zakończeniu zapisywania, używając metody 'end', aby upewnić się, że wszystkie dane zostały poprawnie zapisane.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o tworzeniu plików w języku TypeScript, polecamy zapoznanie się z następującymi artykułami:

- [Dokumentacja oficjalna: Moduł fs w TypeScript](https://www.typescriptlang.org/docs/handbook/nodejs.html#rest-parameters)
- [Blog TypeScript na Medium](https://medium.com/@typescript)
- [Kurs online: Wprowadzenie do TypeScript](https://www.udemy.com/course/typescript-the-complete-developers-guide/)