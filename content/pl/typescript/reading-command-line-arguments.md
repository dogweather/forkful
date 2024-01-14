---
title:    "TypeScript: Odczytywanie argumentów z wiersza poleceń"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak niektóre programy przyjmują argumenty wprowadzone z wiersza poleceń? Czy chcesz nauczyć się tego, jak osiągnąć to w swoim kodzie TypeScript? Ten artykuł jest dla Ciebie! Poznasz podstawy odczytywania argumentów wiersza poleceń i później możesz je wykorzystać w swoich własnych projektach.

## Jak To Zrobić

 ```TypeScript
 // Przykładowy kod odczytujący argumenty wiersza poleceń
 const args = process.argv.slice(2);

 for (let arg of args) {
   console.log(arg);
 }
 ```
Podczas uruchamiania powyższego kodu z argumentami, takimi jak `node script.ts hello world`, otrzymamy następujący wynik:

```Shell
hello
world
```

Kod ten wykorzystuje obiekt `process`, który jest wbudowany w środowisko Node.js, aby uzyskać dostęp do argumentów wiersza poleceń. Metoda `slice` jest używana, aby pominąć pierwsze dwa argumenty, które zawsze są zarezerwowane dla ścieżki dostępu do pliku i polecenia Node.js.

## Deep Dive

W przypadku bardziej złożonych programów możesz chcieć wykorzystać moduł `yargs`, który ułatwia przetwarzanie argumentów wiersza poleceń i wyświetlanie przydatnej pomocy w razie potrzeby. Możesz również stosować różne walidatory do wprowadzanych argumentów, aby upewnić się, że są one zgodne z oczekiwaniami.

Valoryzacja argumentów to także dobry sposób na zabezpieczenie programu przed nieprawidłowym użyciem. Dzięki temu możesz uniknąć błędów w działaniu programu lub ewentualnych ataków na oprogramowanie.

## Zobacz Również

- [Dokumentacja Node.js - Obiekt process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [Dokumentacja yargs](https://www.npmjs.com/package/yargs)
- [Przydatny samouczek dotyczący argumentów wiersza poleceń w Node.js](https://www.digitalocean.com/community/tutorials/how-to-read-arguments-in-node-js)
- [Wideo z poradami na temat pracy z argumentami wiersza poleceń w TypeScript](https://www.youtube.com/watch?v=B2zd56jL3XY)