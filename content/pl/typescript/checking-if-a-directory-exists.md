---
title:    "TypeScript: Sprawdzanie istnienia katalogu"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy katalog istnieje, jest ważną umiejętnością w programowaniu TypeScript. Dzięki tej umiejętności możemy upewnić się, czy przed próbą wykonania pewnych operacji na plikach, istnieje odpowiedni katalog, w którym będą się one odbywać. W tym artykule pokażę Ci, jak to zrobić.

## Jak to zrobić

Sprawdzenie, czy katalog istnieje, jest w rzeczywistości bardzo prostym zadaniem w TypeScript, dzięki wbudowanej metodzie `existsSync ()` z modułu `fs`. Poniżej znajduje się przykładowy kod, w którym sprawdzamy, czy katalog o nazwie "projekt" istnieje:

```TypeScript
import fs from 'fs';

if (fs.existsSync('./projekt')) {
    console.log('Katalog "projekt" istnieje.');
} else {
    console.log('Katalog "projekt" nie istnieje.');
}
```

W przypadku, gdy katalog "projekt" istnieje, otrzymamy w konsoli wiadomość "Katalog "projekt" istnieje.", a w przeciwnym wypadku "Katalog "projekt" nie istnieje.". Możesz również wykorzystać tę metodę w warunkach, na przykład:

```TypeScript
fs.existsSync('./projekt') ? console.log('Katalog "projekt" istnieje.') : console.log('Katalog "projekt" nie istnieje.');
```

## Pogłębiona analiza

Metoda `existsSync()` zwraca wartość logiczną typu `boolean`, więc możemy ją wykorzystać w warunkach jak w przykładach powyżej. Należy jednak pamiętać, że ta metoda jest synchroniczna, więc może przeszkadzać w wydajności naszej aplikacji w przypadku sprawdzania wielu katalogów.

Jeśli chcesz dowiedzieć się więcej o zarządzaniu plikami i katalogami w TypeScript, możesz zapoznać się z dokumentacją dotyczącą modułu `fs` lub skorzystać z innych przydatnych zasobów, które znajdziesz w sekcji "Zobacz również" poniżej.

## Zobacz również

- Dokumentacja dotycząca modułu `fs`: https://nodejs.org/api/fs.html
- Artykuł o zarządzaniu plikami i katalogami w TypeScript: https://blog.logrocket.com/handling-files-with-node-js/
- Wideo tutorial na temat operacji na plikach w TypeScript: https://www.youtube.com/watch?v=SctPZaDdXgY