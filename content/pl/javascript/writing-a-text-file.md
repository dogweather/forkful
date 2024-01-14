---
title:                "Javascript: Redagowanie pliku tekstowego"
simple_title:         "Redagowanie pliku tekstowego"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego może być przydatną umiejętnością dla każdego programisty Javascript. Pozwala ono na zapisanie danych w formie nie tylko kodu, ale również tekstów, list czy tabel. Jest to ważny krok w procesie tworzenia wielu aplikacji, więc warto nauczyć się tej umiejętności.

## Jak to zrobić

Do napisania pliku tekstowego w Javascript potrzebujemy wykorzystać wbudowany moduł "fs", który odpowiada za operacje na plikach. Najpierw musimy zaimportować ten moduł do naszego kodu, aby móc z niego korzystać. Następnie wykorzystujemy metodę "writeFileSync", która przyjmuje jako argumenty ścieżkę do nowego pliku oraz dane, które chcemy w nim zapisać. Pozwala nam to na utworzenie nowego pliku lub nadpisanie już istniejącego.

Poniżej przedstawiam przykładowy kod, który stworzy plik tekstowy o nazwie "moj_plik.txt" i zapisze w nim tekst "Witaj, to jest mój pierwszy plik tekstowy w Javascript!"

```Javascript
const fs = require('fs');
fs.writeFileSync("moj_plik.txt", "Witaj, to jest mój pierwszy plik tekstowy w Javascript!");
```

Po uruchomieniu powyższego kodu, w folderze, w którym znajduje się nasz projekt, powinien pojawić się nowy plik tekstowy o nazwie "moj_plik.txt" zawierający tekst, który podaliśmy jako drugi argument.

## Głębsza analiza

Jak już wspomniano, "fs" jest wbudowanym modułem w Javascript, więc nie musimy instalować żadnych dodatkowych paczek. Posiada on wiele innych metod, takich jak "readFileSync" czy "appendFileSync", które pozwalają na czytanie oraz dodawanie danych do istniejących plików tekstowych.

Warto również pamiętać o wyjątkach, które mogą wystąpić przy próbie utworzenia lub zapisu pliku. Dobrą praktyką jest umieszczenie kodu w bloku "try-catch", co pozwala na obsłużenie błędów w przypadku niepowodzenia.

## Zobacz także

Teraz, kiedy już wiesz jak w prosty sposób można napisać plik tekstowy w Javascript, zachęcam do zapoznania się z innymi modułami dostępnymi w Node.js, które pozwalają na jeszcze większą manipulację plikami. Poniżej znajdują się przydatne linki:

- Oficjalna dokumentacja wbudowanego modułu "fs" w Node.js: https://nodejs.org/api/fs.html
- Popularny moduł "fs-extra", który dodaje wiele przydatnych funkcji do już istniejącego "fs": https://www.npmjs.com/package/fs-extra
- Poradnik na temat manipulacji plikami w Node.js: https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm