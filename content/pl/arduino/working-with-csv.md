---
title:                "Praca z plikami csv"
html_title:           "Arduino: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz przetworzyć duże ilości danych w swoim projekcie Arduino, wykorzystanie plików CSV może ułatwić Ci pracę. Znajomość pracy z tym formatem danych może znacznie ułatwić analizę i wykorzystanie informacji w Twoich projektach.

## Jak to zrobić?

### Przygotowanie środowiska

Aby móc pracować z CSV w Arduino, musisz najpierw zainstalować bibliotekę TinyCSVParser. Zrób to za pomocą menedżera bibliotek w Arduino IDE lub ręcznie pobierając plik .zip ze strony projektu na GitHubie.

### Tworzenie pliku CSV

Pierwszym krokiem jest stworzenie pliku CSV z danymi. Możesz to zrobić w programie Excel lub innej aplikacji do arkuszy kalkulacyjnych. Pamiętaj, aby oddzielać każdą wartość przecinkiem.

### Przygotowanie kodu

W pierwszej kolejności musisz zaimportować bibliotekę TinyCSVParser w swoim kodzie:

```Arduino
#include <TinyCSVParser.h>
```

Następnie zdefiniujesz strukturę danych, która będzie odpowiadać wartościom z pliku CSV:

```Arduino
struct row{
  int id;
  String name;
  float value;
};
```

Teraz stwórz tablicę, która będzie przechowywać odczytane dane z pliku CSV:

```Arduino
row csvData[10]; // 10 elementów odpowiada 10 wierszom pliku CSV
```

### Odczyt danych

Aby odczytać dane z pliku CSV, musisz utworzyć obiekt parsera:

```Arduino
CsvParser<row> parser;
```

Następnie otwórz plik CSV i przekaż jego zawartość do parsera:

```Arduino
File csvFile = SD.open("example.csv", FILE_READ);
parser.parse(csvFile, csvData);
```

W ten sposób, w tablicy `csvData` znajdują się przetworzone dane z pliku CSV. Możesz wyświetlić je na serial monitorze przy użyciu pętli for:

```Arduino
for(int i = 0; i < 10; i++){
  Serial.println(csvData[i].id);
  Serial.println(csvData[i].name);
  Serial.println(csvData[i].value);
}
```

### Tworzenie pliku CSV

Jeśli chcesz również móc tworzyć pliki CSV w swoim projekcie, to również możesz skorzystać z biblioteki TinyCSVParser. Stwórz obiekt writer i przekaż mu informacje o pliku, w którym chcesz zapisać dane:

```Arduino
CsvWriter<row> writer;
File csvFile = SD.open("data.csv", FILE_WRITE);
writer.init(csvFile);
```

Następnie, wykorzystując funkcję `writeRow()`, można zapisać strukturę danych do pliku:

```Arduino
row data = {1, "example", 1.5}; // wartości testowe
writer.writeRow(data); // zapisz dane jako jeden wiersz w pliku CSV
```

## Deep Dive

Biblioteka TinyCSVParser pozwala również dostosować sposób przetwarzania danych z pliku CSV. Można na przykład zmienić separatory lub wykorzystać własną funkcję przetwarzającą dane. W celu uzyskania szczegółowych informacji, warto zapoznać się z dokumentacją biblioteki i przykładowymi kodami na GitHubie.

## Zobacz również

- Dokumentacja TinyCSVParser: https://github.com/kevinferrare/TinyCSVParser
- Przykładowe kody z wykorzystaniem TinyCSVParser: https://github.com/kevinferrare/TinyCSVParser/wiki/Code-Snippets
- Strona projektu TinyCSVParser na GitHubie: https://github.com/kevinferrare/TinyCSVParser