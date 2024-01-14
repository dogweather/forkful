---
title:    "Javascript: Sprawdzenie istnienia katalogu"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy katalog istnieje, jest ważnym aspektem w pisaniu skryptów JavaScript. Może to pomóc programistom w uniknięciu błędów i zapewnieniu poprawnego działania aplikacji.

## Jak to zrobić

Aby sprawdzić istnienie katalogu w JavieScript, możemy użyć funkcji `existsSync()` z modułu `fs`. Poniższy przykład pokazuje jak to zrobić:

```Javascript
const fs = require('fs');

const directoryPath = "/ścieżka/do/katalogu";

if (fs.existsSync(directoryPath)) {
    console.log("Katalog istnieje");
} else {
    console.log("Katalog nie istnieje");
}
```

Jeśli katalog istnieje, wyświetli się komunikat "Katalog istnieje", w przeciwnym razie wyświetli się komunikat "Katalog nie istnieje".

## Głębsze zagłębienie

Funkcja `existsSync()` wykonuje jej działanie synchronicznie, co oznacza, że ​​będzie blokować wykonywanie innych funkcji do czasu zakończenia sprawdzania istnienia katalogu. W przypadku większych aplikacji może to spowolnić działanie programu, dlatego zaleca się używanie funkcji `exists()` z modułu `fs` zamiast `existsSync()`, która działa asynchronicznie.

Kolejnym ważnym aspektem jest obsługa błędów. Aby uniknąć nieoczekiwanych zachowań aplikacji, powinniśmy uwzględnić obsługę błędów w naszym kodzie. Możemy użyć konstrukcji `try-catch` do przechwycenia i obsługi błędów w naszej funkcji.

## Zobacz również

- [Dokumentacja Node.js na temat modułu fs](https://nodejs.org/api/fs.html)
- [Poradnik na temat sprawdzania istnienia katalogu w JavieScript](https://www.digitalocean.com/community/tutorials/nodejs-check-if-file-exists)