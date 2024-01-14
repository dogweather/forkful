---
title:                "TypeScript: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Dlaczego tworzymy tymczasowe pliki?

Tworzenie tymczasowych plików jest nieodłączną częścią programowania w TypeScript. Pomimo, że możemy wykonywać wiele operacji na zawartości plików, niektóre z nich wymagają tworzenia tymczasowych plików, aby przetworzyć dane w bardziej efektywny sposób. W tym artykule dowiesz się, jak stworzyć tymczasowy plik w TypeScript oraz w jaki sposób wykorzystać go w swoim kodzie.

##Jak stworzyć tymczasowy plik w TypeScript?

Aby stworzyć tymczasowy plik w TypeScript, możemy użyć modułu `fs` i jego funkcji `mktempSync()`. Poniższy kod pokazuje przykładowe użycie tej funkcji w TypeScript:

```TypeScript
import fs from 'fs';

const temporaryFilePath = fs.mktempSync('tempFile-XXXXXX.txt');
```

Funkcja `mktempSync()` przyjmuje jako argument wzorzec nazwy tymczasowego pliku, w którym znaki `X` są zastępowane losowymi znakami. Warto zauważyć, że ta funkcja automatycznie tworzy plik, więc nie musimy sami wywoływać funkcji `fs.createWriteStream()`.

Po wykonaniu tego kodu, nowy tymczasowy plik zostanie utworzony pod podaną nazwą wzorca, a jego ścieżka zostanie zwrócona przez funkcję `mktempSync()`. Możemy teraz wykorzystać tę ścieżkę do wykonania operacji na naszym pliku.

##Głębsze zanurzenie

Tworzenie tymczasowych plików jest użyteczne w różnych przypadkach, takich jak przetwarzanie danych w pamięci podręcznej lub tworzenie plików tymczasowych do testowania naszego kodu. W wielu przypadkach możemy również użyć funkcji `mkdtempSync()`, która tworzy tymczasowy katalog z podobnym wzorcem nazwy.

Ponadto, funkcje `mktempSync()` i `mkdtempSync()` przyjmują drugi argument, który pozwala na określenie lokalizacji, w której zostanie utworzony tymczasowy plik lub katalog. Dzięki temu, możemy kontrolować, gdzie nasze tymczasowe pliki będą przechowywane.

## Zobacz również

- Dokumentacja fs.mktempSync() (https://nodejs.org/api/fs.html#fs_fs_mktempsync_template_options)
- Dokumentacja fs.mkdtempSync() (https://nodejs.org/api/fs.html#fs_fs_mkdtempsync_prefix_options)