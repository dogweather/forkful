---
title:                "TypeScript: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego?

Każdy programista wie, że w świecie programowania czasem trzeba mieć możliwość dostosowania działania programu w zależności od zmiennych warunków zewnętrznych. W przypadku aplikacji konsolowych, jednym z najprostszych sposobów na to jest używanie argumentów wiersza poleceń. Pozwala to na przekazywanie programowi informacji w momencie wywołania go, co może znacznie ułatwić pracę i uczynić program bardziej elastycznym. W tym artykule dowiesz się, jak czytać argumenty wiersza poleceń w języku TypeScript.

## Jak to zrobić?

Aby odczytać argumenty wiersza poleceń w TypeScript, najpierw musimy zainstalować i skonfigurować odpowiednie narzędzia. Przede wszystkim, potrzebujemy odpowiedniej wersji Node.js na naszej maszynie. Następnie, możemy zainstalować pakiet commander za pomocą managera pakietów npm, wpisując w terminalu komendę:

```TypeScript
npm install commander
```

Teraz, możemy przejść do tworzenia pliku TypeScript, w którym będziemy czytać argumenty wiersza poleceń. Pamiętaj, aby ustawić konfigurację kompilatora, aby wykorzystywał moduł "commonjs". Następnie, możemy zaimportować pakiet commander i utworzyć nową instancję klasy Command. W jej konstruktorze, możemy przekazać aliasy oraz opisy argumentów, które chcemy obsługiwać. Na przykład:

```TypeScript
import {Command} from 'commander';

const program = new Command();

program
  .option('-f, --file <filePath>', 'ścieżka do pliku')
  .option('-d, --debug', 'włącz tryb debugowania')
  .parse(process.argv);
```

Na koniec, wywołujemy metodę `parse()`, przekazując jej tablicę argumentów wiersza poleceń, otrzymaną dzięki obiektowi `process`, który znajduje się w module "node". Teraz, możemy odczytać wartości naszych argumentów korzystając z właściwości obiektu `program`. Na przykład, jeśli użytkownik wpisze w terminalu komendę "node index.js -f test.txt -d", będziemy mogli odwołać się do wartości argumentów poleceniami `program.file` oraz `program.debug`.

## Deep Dive

Powyżej przedstawiliśmy najprostszy przykład użycia pakietu commander. Jednak, biblioteka ta oferuje wiele innych funkcjonalności, takich jak obsługa wyjątków czy walidacja argumentów. Możliwości jest wiele, a dokładny opis ich działania można znaleźć w oficjalnej dokumentacji.

## Zobacz też

- [Oficjalna dokumentacja pakietu commander](https://www.npmjs.com/package/commander)
- [Dokumentacja dla modułu "node" - obiekt process](https://nodejs.org/dist/latest-v8.x/docs/api/process.html)