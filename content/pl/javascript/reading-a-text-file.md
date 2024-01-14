---
title:    "Javascript: Odczytywanie pliku tekstowego"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele sytuacji, w których jesteśmy zmuszeni do czytania plików tekstowych w naszych programach Javascript. Może to być konieczne do wczytania danych do naszej aplikacji lub do przetworzenia pliku z danymi. Bez względu na cel, musimy wiedzieć jak czytać pliki tekstowe za pomocą Javascript.

## Jak to zrobić

Najprostszym sposobem na czytanie plików tekstowych jest wykorzystanie wbudowanego obiektu "fs" (file system) w Javascript. Potrzebujemy najpierw zaimportować ten obiekt, a następnie możemy wykorzystać funkcję "readFileSync()" do wczytania pliku. Poniżej znajduje się przykładowy kod:

```Javascript
const fs = require('fs'); // importowanie obiektu
const text = fs.readFileSync('nazwa_pliku.txt', 'utf8'); // wczytywanie pliku
console.log(text); // wyświetlanie zawartości pliku
```

Powyższy kod wczytuje zawartość pliku "nazwa_pliku.txt" i wypisuje ją w konsoli. Ważne jest również zauważyć, że wykorzystaliśmy opcję 'utf8' w funkcji "readFileSync()", która określa format kodowania tekstu w pliku.

Jeśli chcielibyśmy przetworzyć zawartość pliku w celu dalszej obróbki, możemy wykorzystać funkcję "split()" wraz z odpowiednim separatorrem, na przykład znakiem nowej linii:

```Javascript
const lines = text.split('\n'); // dzielenie tekstu na linie
for (let i = 0; i < lines.length; i++) { // pętla przetwarzająca kolejne linie
    console.log(lines[i]);
}
```

## Deep Dive

Gdy już umiemy wczytywać i przetwarzać zawartość plików tekstowych w Javascript, warto również poznać bardziej zaawansowane techniki. Możemy na przykład wykorzystać moduł "readline", który umożliwia nam czytanie plików tekstowych linia po linii, co jest szczególnie przydatne przy przetwarzaniu większych plików. Możemy również wykorzystać technikę streamingu, która pozwala na wczytywanie pliku w częściach, co jest korzystne dla wydajności i nie obciąża pamięci naszego programu.

## Zobacz też

Poniżej znajduje się lista przydatnych linków dla dalszej nauki:

- [Dokumentacja wbudowanego modułu "fs" w Javascript](https://nodejs.org/api/fs.html)
- [Dokumentacja modułu "readline"](https://nodejs.org/api/readline.html)
- [Poradnik dotyczący streamingu w Javascript](https://www.infoq.com/articles/nodejs-streams/)

Teraz już wiesz jak czytać pliki tekstowe w programach Javascript. Bądź ciekawski i eksperymentuj z różnymi metodami, aby dostosować je do swoich potrzeb. Powodzenia!