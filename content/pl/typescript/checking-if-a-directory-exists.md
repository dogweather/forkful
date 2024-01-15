---
title:                "Sprawdzanie, czy istnieje katalog."
html_title:           "TypeScript: Sprawdzanie, czy istnieje katalog."
simple_title:         "Sprawdzanie, czy istnieje katalog."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie istnienia katalogu jest ważnym elementem w programowaniu. Wiele aplikacji wymaga dostępu do określonego katalogu w celu odczytu lub zapisu danych. Dlatego ważne jest, aby móc sprawdzić, czy katalog istnieje przed próbą dostępu do niego.

## Jak To Zrobić

Sprawdzenie istnienia katalogu w TypeScript jest proste i wymaga użycia wbudowanej funkcji `existsSync` z modułu `fs`. Przykładowy kod przedstawiający sprawdzenie istnienia katalogu "test":

```TypeScript
import * as fs from "fs";

if (fs.existsSync("test")) {
  console.log("Katalog istnieje.");
} else {
  console.log("Katalog nie istnieje.");
}
```

W powyższym przykładzie, jeśli katalog "test" istnieje, na ekranie zostanie wyświetlone "Katalog istnieje." W przeciwnym razie zostanie wyświetlone "Katalog nie istnieje." Można również użyć asynchronicznej wersji funkcji `exists`, która przyjmuje dwa parametry: ścieżkę do katalogu oraz funkcję zwrotną (callback). Przykład:

```TypeScript
fs.exists("test", (exists) => {
  if (exists) {
    console.log("Katalog istnieje.");
  } else {
    console.log("Katalog nie istnieje.");
  }
});
```

## Deep Dive

Funkcja `existsSync` zwraca wartość logiczną `true` lub `false` w zależności od istnienia katalogu. W przypadku używania asynchronicznej wersji, funkcja zwrotna przyjmuje parametr `exists`, który również jest wartością logiczną. 

W przypadku, gdy katalog nie istnieje, spróbujemy uzyskać dostęp do niego przez funkcję `readdirSync` lub `readdir` (w przypadku wersji asynchronicznej). Oba typy funkcji służą do odczytu zawartości katalogu. Jeśli katalog nie istnieje, zostanie wygenerowany wyjątek, który można obsłużyć przy użyciu bloku `try-catch`. Przykład:

```TypeScript
try {
  const files = fs.readdirSync("test");
  console.log(files);
} catch (err) {
  console.error(err);
}
```

## Zobacz także

- Dokumentacja fs: https://nodejs.org/api/fs.html
- Inne przydatne metody w module fs: https://nodejs.org/api/fs.html#fs_fs_accesssync_path_mode
- Wideo tutorial o проверке существования каталога: https://www.youtube.com/watch?v=MoSsYEFwZJ8