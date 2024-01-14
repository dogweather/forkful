---
title:    "TypeScript: Tworzenie pliku tekstowego"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest niezbędne dla każdego, kto pracuje z kodem. Pozwala to na przechowywanie i udostępnianie swoich programów, bez obawy o utratę kodu.

## Jak to zrobić

Aby zapisać plik tekstowy za pomocą TypeScript, wystarczy użyć wbudowanej funkcji `writeFileSync` z modułu `fs`. Przykładowy kod wyglądałby następująco:

```TypeScript
import * as fs from 'fs';

fs.writeFileSync('plik.txt', 'To jest przykładowy tekst.');
```

Po uruchomieniu kodu, w bieżącym folderze pojawi się plik tekstowy `plik.txt`, zawierający tekst "To jest przykładowy tekst.". Jeśli chcesz dodać tekst do już istniejącego pliku, możesz użyć funkcji `appendFileSync` zamiast `writeFileSync`.

```TypeScript
import * as fs from 'fs';

fs.appendFileSync('plik.txt', '\nTo jest kolejny tekst.');
```

Teraz plik będzie zawierał dwa wiersze tekstu: "To jest przykładowy tekst." i "To jest kolejny tekst.".

## Deep Dive

Istnieje wiele opcji, które możesz podać jako drugi parametr funkcji `writeFileSync` lub `appendFileSync`, aby dostosować sposób zapisu pliku. Możesz na przykład określić kodowanie znaków, tryb zapisu, a nawet funkcję zwrotną, która zostanie wywołana po zapisaniu pliku.

```TypeScript
import * as fs from 'fs';

fs.writeFileSync('plik.txt', 'To jest przykładowy tekst.', { encoding: 'utf-8', mode: 0o666 }, () => {
    console.log('Plik został zapisany.');
});
```

W powyższym przykładzie, zostanie zapisany plik tekstowy o nazwie `plik.txt` z kodowaniem UTF-8 i trybem zapisu 0o666 (czyli wszystkie prawa dostępu). Po zapisaniu, zostanie wywołana funkcja zwrotna, która wypisze w konsoli komunikat "Plik został zapisany.".

## Zobacz również

- [Oficjalna dokumentacja TypeScript](https://www.typescriptlang.org/docs/home.html)
- [Wprowadzenie do programowania w TypeScript](https://codeburst.io/a-beginners-guide-to-typescript-13b33062580b?source=language_selector)