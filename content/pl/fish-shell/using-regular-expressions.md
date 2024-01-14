---
title:                "Fish Shell: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub osobą, która często pracuje z tekstowymi plikami, prawdopodobnie słyszałeś(aś) już o wyrażeniach regularnych. Są to bardzo przydatne narzędzia w programowaniu, które pozwalają na wykonywanie zaawansowanych operacji na tekście, takich jak wyszukiwanie, zastępowanie lub filtrowanie. Jeśli chcesz nauczyć się, jak efektywnie wykorzystywać wyrażenia regularne w języku Fish Shell, zapraszamy do lektury!

## Jak To Zrobić

Aby używać wyrażeń regularnych w języku Fish Shell, musisz najpierw zapoznać się z ich składnią i podstawowymi operacjami. W skrócie, wyrażenia regularne są wyrażeniami, które pozwalają na wykonywanie operacji wyszukiwania i manipulacji na tekście, wykorzystując określone wzory. Dzięki nim możesz znaleźć określone frazy w tekście, np. wszystkie adresy email lub numery telefonów, lub zamienić jedne wyrazy na inne. Oto kilka przykładowych zastosowań wyrażeń regularnych w języku Fish Shell:

```Fish Shell
set files (ls *.txt) # utworzenie listy plików z rozszerzeniem .txt
for file in $files
    cat $file | grep -q "lorem ipsum" # wyszukanie w pliku frazy "lorem ipsum"
        if test $status = 0
        echo $file # jeśli fraza została znaleziona, wyświetl nazwę pliku
        end
end
```

```Fish Shell
set text "załącznik_1.txt, załącznik_2.txt, załącznik_3.txt" # przykładowy tekst zawierający nazwy plików
echo $text | sed -E 's/([a-zćęółśżź]+)_/Pliki: \1, /g' # zamiana nazw plików na formę zdania, przykładowy output: "Pliki: załącznik_1.txt, załącznik_2.txt, załącznik_3.txt"
```

Jak widać, wyrażenia regularne są niezwykle przydatne w manipulowaniu tekstem. Aby bardziej zrozumieć składnię i zastosowania wyrażeń regularnych w języku Fish Shell, polecamy zapoznać się z dokumentacją i wypróbować różne przykłady.

## Zanurz się Głębiej

Jeśli chcesz poznać wyrażenia regularne jeszcze lepiej, warto poświęcić trochę czasu na zgłębienie tematu. Na przykład możesz nauczyć się wykorzystywać specjalne znaki i metaznaki, które pozwalają na bardziej zaawansowane operacje, takie jak wykluczanie lub dopasowywanie grup tekstu. W języku Fish Shell możesz również wykorzystać wyrażenia regularne do manipulowania zmiennymi i pętlami.

## Zobacz także

- Dokumentacja języka Fish Shell: https://fishshell.com/docs/current/
- Tutoriale i przykłady wyrażeń regularnych: https://www.regular-expressions.info/ 
- Interaktywny edytor wyrażeń regularnych: https://regex101.com/