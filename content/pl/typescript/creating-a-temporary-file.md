---
title:    "TypeScript: Tworzenie tymczasowego pliku"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego może być kluczową częścią procesu programowania w TypeScript. Jest to przydatna technika, która pozwala programistom na tymczasowe przechowywanie danych, aby móc je później przetwarzać lub wykorzystywać w swoich programach. To szczególnie przydatne, gdy potrzebne są tymczasowe dane, których nie chcemy trwale zapisać na dysku. W tym artykule dowiesz się, jak w prosty sposób tworzyć pliki tymczasowe w TypeScript i wykorzystywać je w swoim kodzie.

## Jak

Aby utworzyć plik tymczasowy w TypeScript, użyjemy wbudowanego modułu "fs". Ten moduł zawiera wiele funkcji, które umożliwiają nam operacje na plikach i folderach. Najpierw musimy zaimportować ten moduł do naszego kodu, używając polecenia "require":

```TypeScript
const fs = require('fs');
```

Następnie możemy użyć funkcji "fs.mkdtemp()" do utworzenia tymczasowego folderu, w którym będzie znajdował się nasz plik tymczasowy. W tym przykładzie użyjemy prefiksu "temp-" w nazwie naszego folderu, ale można go zmienić na dowolny inny prefiks.

```TypeScript
let folder = fs.mkdtempSync('temp-');
```

Teraz, gdy mamy nasz tymczasowy folder, możemy stworzyć w nim plik używając funkcji "fs.writeFile()". Będziemy musieli określić nazwę pliku oraz treść, którą chcemy w nim zapisać.

```TypeScript
let fileName = folder + '/example.txt';
fs.writeFile(fileName, 'To jest przykładowa treść naszego pliku tymczasowego', (err) => {
    if (err) throw err;
    console.log('Plik tymczasowy został utworzony pomyślnie!');
});
```

Teraz można przetwarzać nasze tymczasowe dane z pliku, a po zakończeniu pracy usuń tymczasowy folder i plik z dysku. Aby to zrobić, użyjemy funkcji "fs.unlink()" i "fs.rmdir()".

```TypeScript
// usuwamy plik tymczasowy
fs.unlink(fileName, (err) => {
    if (err) throw err;
    console.log('Plik tymczasowy został usunięty pomyślnie!');
});

// usuwamy tymczasowy folder
fs.rmdir(folder, (err) => {
    if (err) throw err;
    console.log('Tymczasowy folder został usunięty pomyślnie!');
});
```

Jako wynik otrzymamy w konsoli komunikaty potwierdzające utworzenie, a następnie usunięcie pliku tymczasowego.

## Deep Dive

Podczas tworzenia pliku tymczasowego można dostosować niektóre z jego parametrów, takie jak nazwa, lokalizacja czy rozszerzenie. Można to zrobić poprzez ustawienie odpowiednich opcji w funkcji "fs.mkdtemp()". Na przykład, aby ustawić lokalizację dla naszego tymczasowego folderu, możemy użyć opcji "encoding":

```TypeScript
let options = { encoding: 'utf8', dir: 'tmp/' };
let folder = fs.mkdtempSync('temp-', options);
```

Możemy również wygenerować losowe numery lub litery do nazw plików tymczasowych, używając opcji "prefix" oraz "postfix". Jest to przydatne, gdy chcemy tworzyć wiele plików tymczasowych naraz.

```TypeScript
let options = { prefix: 'temp-', postfix: '.txt' };
let fileName = fs.mkdtempSync('example', options);
```

Dodatkowo, można skonfigurować uprawnienia dla plików tymczasowych, dzięki czemu można ustalić, kto ma dostęp do tych danych. Jest to szczególnie ważne, gdy pracujemy z poufnymi informacjami.

```TypeScript