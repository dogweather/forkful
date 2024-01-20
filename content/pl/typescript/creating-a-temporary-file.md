---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Tworzenie tymczasowego pliku polega na utworzeniu pliku, który służy do przechowywania danych na krótki czas. Programiści robią to zwykle, gdy potrzebują przechować dane, które są tylko tymczasowo potrzebne lub które chcą chronić przed utratą w przypadku awarii.

## Jak to zrobić:
Tworzenie tymczasowych plików w TypeScript jest proste. Możemy skorzystać z pakietu "tmp", który dostarcza wygodne funkcje do tego zadania. Instalujemy go za pomocą npm przy użyciu komendy `npm install tmp` i można go użyć jak poniżej.

```TypeScript
import { fileSync } from 'tmp';

let tmpobj = fileSync();
console.log('File: ', tmpobj.name);
console.log('Filedescriptor: ', tmpobj.fd);;
```

Przy wykonaniu tego kodu, stworzony zostanie unikalny tymczasowy plik i jego nazwa zostanie wyświetlona na konsoli.

## Pogłębione informacje
Stworzenie tymczasowego pliku nie jest nowym konceptem. Zostało to wprowadzone w starszych systemach operacyjnych, takich jak Unix, gdzie takie pliki były przechowywane w `/tmp` lub `/var/tmp`. 

Jedną z alternatyw w TypeScript jest używanie modułu `fs` w Node.js, który również oferuje funkcje do tworzenia i manipulacji plikami. Choć jest to bardziej niskopoziomowe podejście, daje to większą kontrolę nad tworzeniem plików.

Pakiet "tmp" używa `fs` pod spodem, ale zapewnia większą wygodę i bezpieczeństwo, takie jak automatyczne usuwanie pliku po zakończeniu programu.

## Zobacz również
1. [Pakiet tmp na npm](https://www.npmjs.com/package/tmp)
2. [Moduł fs w Node.js](https://nodejs.org/api/fs.html)