---
title:                "TypeScript: Odczytywanie pliku tekstowego"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego czytanie plików tekstowych jest przydatne w programowaniu TypeScript?

Programowanie to sztuka tworzenia nowych i innowacyjnych aplikacji, jednak czasem musimy również pracować z istniejącymi danymi. Często potrzebujemy odczytać dane z plików tekstowych, na przykład danych użytkowników lub konfiguracji aplikacji. W tym artykule dowiecie się, dlaczego czytanie plików tekstowych jest ważnym aspektem programowania w TypeScript oraz jak to zrobić.

## Jak to zrobić

Aby odczytać plik tekstowy w TypeScript, możemy skorzystać z wbudowanych funkcji systemu plików lub z biblioteki zewnętrznej, takiej jak "fs-extra". Sprawdźmy przykład użycia funkcji "readFile" z biblioteki "fs-extra":

```TypeScript
import * as fs from 'fs-extra';

fs.readFile('dane.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

W powyższym przykładzie użyliśmy funkcji "readFile" aby odczytać zawartość pliku "dane.txt" w formacie UTF-8. W przypadku wystąpienia błędu, funkcja wyświetli go w konsoli, a w przeciwnym razie wyświetli zawartość pliku.

## Vertigo Dive

Czytanie plików tekstowych może być zagmatwanym procesem, jeśli nie znamy dokładnie struktury pliku lub sposobu jego formatowania. W takiej sytuacji, przydatne może być skorzystanie z narzędzi lub metod, takich jak "split" czy "match", które pozwolą nam na przetworzenie zawartości pliku w sposób bardziej złożony. Żeby zobaczyć to w akcji, przyjrzyjmy się następującemu przykładowi:

```TypeScript
import * as fs from 'fs-extra';

fs.readFile('dane.txt', 'utf8', (err, data) => {
  if (err) throw err;
  let lines = data.split('\n');
  for (let i = 0; i < lines.length; i++) {
    let user = lines[i].match(/(\w+), (\w+), (\d+)/);
    console.log(`Użytkownik ${user[1]} ma ${user[3]} lat.`);
  }
});
```

W powyższym przykładzie użyliśmy funkcji "split" do podzielenia zawartości pliku na poszczególne linie oraz metody "match" do wykonania wyrażenia regularnego na każdej linii, aby wyodrębnić pożądane informacje. Dzięki temu możemy wyświetlić użytkowników i ich wiek w czytelnej formie.

# Zobacz też

- [Dokumentacja biblioteki fs-extra](https://github.com/jprichardson/node-fs-extra)
- [Poradnik dotyczący przetwarzania tekstu w TypeScript](https://blog.logrocket.com/processing-text-in-typescript/)
- [Poradnik dotyczący wyrażeń regularnych w JavaScript](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)