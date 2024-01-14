---
title:                "TypeScript: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV (ang. Comma Separated Values) to bardzo popularny format pliku używany do przechowywania danych tabelarycznych. Jest on wygodny w użyciu i łatwy do wczytania przez programy, przez co stał się podstawowym wyborem dla wielu aplikacji i projektów. W tym artykule dowiesz się, dlaczego warto zainteresować się pracą z plikami CSV.

## Jak to zrobić 

Aby wczytać dane ze pliku CSV w TypeScript, musisz najpierw zainstalować odpowiedni pakiet. Możesz to zrobić za pomocą menedżera pakietów NPM za pomocą poniższej komendy:

```TypeScript
npm install csv-parser
```

Następnie w pliku z kodem TypeScript, musisz zaimportować pakiet oraz moduł `fs`, który pozwoli Ci czytać pliki. Kod może wyglądać następująco:

```TypeScript
import { createReadStream } from "fs";
import csvParser from "csv-parser";

createReadStream("dane.csv")
  .pipe(csvParser())
  .on("data", (row) => {
    console.log(row); //Przetworzone dane
  })
  .on("end", () => {
    console.log("Wczytano plik CSV");
  });
```

Powyższy kod wczytuje plik `dane.csv` i przetwarza go w formie obiektów JavaScript. Możesz teraz w łatwy sposób odwołać się do odpowiednich wartości i wykorzystać je w swoim programie.

## Deep Dive 

Kiedy już opanujesz podstawy wczytywania danych z plików CSV, możesz zabawić się bardziej zaawansowanymi funkcjami. Na przykład, możesz użyć pakietu `fast-csv`, który pozwala na szybsze wczytywanie dużych plików lub modułu `papaparse`, który radzi sobie z różnymi rodzajami separatorów (np. średnikami zamiast przecinków).

Możesz również skorzystać z różnych opcji dostępnych w pakiecie `csv-parser`, takich jak ignorowanie pierwszego wiersza nagłówkowego lub konfigurowanie separatora i cudzysłowów.

## Zobacz również 

- Oficjalna dokumentacja pakietu csv-parser: https://www.npmjs.com/package/csv-parser
- Przykładowe wczytywanie CSV za pomocą pakietu fast-csv: https://www.npmjs.com/package/fast-csv
- Poradnik na temat manipulowania danymi CSV z wykorzystaniem modułu papaparse: https://www.digitalocean.com/community/tutorials/working-with-csv-data-in-node-js