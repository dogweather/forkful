---
title:                "Praca z plikami csv"
html_title:           "TypeScript: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV (ang. Comma-Separated Values) to popularny format danych używany w biznesie i analizie danych. Podczas prac z danymi, często będziesz mieć do czynienia z plikami CSV, dlatego warto poznać sposób ich obsługi w języku typu TypeScript.

## Jak to zrobić?

Tworzenie plików CSV za pomocą TypeScript jest proste i wygodne. Wystarczy, że wykorzystasz wbudowany moduł "fs" (ang. file system) oraz funkcję "writeFile" z biblioteki "csv-parse". Poniżej przedstawiamy przykładowy kod i oczekiwany output:

```TypeScript
import { writeFile } from 'fs';
import { stringify } from 'csv-parse';

const data = [
  { id: 1, name: 'John', email: 'john@example.com' },
  { id: 2, name: 'Jane', email: 'jane@example.com' },
  { id: 3, name: 'Jack', email: 'jack@example.com' }
];

writeFile('users.csv', stringify(data), (err) => {
  if (err) throw err;
  console.log('CSV file created successfully!');
});
```

Po uruchomieniu powyższego kodu, w katalogu pojawi się plik "users.csv" z zawartością:

```
id,name,email
1,John,john@example.com
2,Jane,jane@example.com
3,Jack,jack@example.com
```

Jeśli chcesz odczytać plik CSV, możesz użyć funkcji "parse" z biblioteki "csv-parse":

```TypeScript
import { readFile } from 'fs';
import { parse } from 'csv-parse';

readFile('users.csv', (err, data) => {
  if (err) throw err;
  parse(data, (err, output) => {
    if (err) throw err;
    console.log(output);
  });
});
```

Powinniśmy otrzymać następujący output:

```
[
  ['id', 'name', 'email'],
  ['1', 'John', 'john@example.com'],
  ['2', 'Jane', 'jane@example.com'],
  ['3', 'Jack', 'jack@example.com']
]
```

## Deep Dive

Istnieje wiele bibliotek, które ułatwiają pracę z plikami CSV w języku TypeScript. Jedną z nich jest "fast-csv", która oferuje wydajne rozwiązania dla odczytu i zapisu plików CSV. Możesz również wykorzystać modularny framework "NestJS", który ma wbudowane narzędzia do obsługi plików CSV.

Innym ważnym aspektem pracy z CSV jest obsługa błędów. W przypadku niepoprawnego formatu danych, możesz użyć funkcji "onError" z biblioteki "csv-parse" lub zaimplementować własny mechanizm obsługi błędów.

## Zobacz też

- [Dokumentacja Node.js o pracy z plikami CSV](https://nodejs.org/api/fs.html#fs_file_system)
- [NestJS: Working with CSV files](https://docs.nestjs.com/techniques/file-upload#csv-files)