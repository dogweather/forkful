---
title:    "Javascript: Odczytywanie pliku tekstowego"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Dlaczego

Dlaczego ktoś mógłby zainteresować się czytaniem pliku tekstowego? Często w programowaniu musimy zwracać uwagę na dane pobrane z zewnętrznych źródeł, takich jak pliki tekstowe. W tym wpisie dowiecie się, w jaki sposób można czytać pliki tekstowe za pomocą JavaScript.

## Jak to zrobić

Czytanie pliku tekstowego z wykorzystaniem JavaScript jest proste i skuteczne. Najpierw musimy wybrać odpowiedni sposób dostępu do pliku. Istnieją dwa sposoby:

1. Synchroniczny - czyli odczytujemy plik w sposób blokujący, co oznacza, że skrypt musi poczekać na zakończenie operacji odczytu, zanim przejdzie do kolejnych linii kodu. 

2. Asynchroniczny - czyli odczytujemy plik w sposób nieblokujący, co oznacza, że skrypt nie musi czekać na zakończenie operacji odczytu i jest w stanie wykonywać inne zadania jednocześnie.

Wybór zależy od indywidualnych preferencji programisty i potrzeb danej aplikacji. Poniżej przedstawione są przykłady obydwu sposobów.

### Synchroniczny odczyt pliku

Aby odczytać plik tekstowy synchronicznie, wystarczy użyć funkcji `readFileSync()` z modułu `fs`. Poniższy przykład kodu demonstruje prosty odczyt pliku tekstowego o nazwie `dane.txt` i wyświetlenie jego zawartości w konsoli.

```Javascript
const fs = require('fs');

const dane = fs.readFileSync('dane.txt', 'utf8');
console.log(dane);

// Output:
// To jest przykładowy plik tekstowy.
```

### Asynchroniczny odczyt pliku

Aby odczytać plik tekstowy asynchronicznie, musimy użyć funkcji `readFile()` z modułu `fs`. Poniższy przykład kodu demonstruje odczyt pliku tekstowego o nazwie `dane.txt` i wyświetlenie jego zawartości w konsoli.

```Javascript
const fs = require('fs');

fs.readFile('dane.txt', 'utf8', (err, data) => {
    if (err) throw err;
    console.log(data);
});

// Output:
// To jest przykładowy plik tekstowy.
```

W obu przypadkach odczytywany jest plik tekstowy w formacie UTF-8. Jeśli plik ten ma inny format, można go wybrać na podstawie potrzeb aplikacji.

## Deeper Dive

Odczytywanie plików tekstowych przy użyciu JavaScript to tylko wierzchołek góry lodowej. W praktyce często musimy dokonać bardziej skomplikowanych operacji na danych pobranych z pliku. Możemy na przykład chcieć przefiltrować dane lub przetworzyć je w pewien sposób. W takich przypadkach przydatne mogą się okazać narzędzia takie jak biblioteka `lodash`, która udostępni nam szereg funkcji do manipulacji danymi.

Możemy również wykorzystać moduł `path` w celu dokonania operacji na ścieżkach plików, co może być pomocne przy odczytywaniu plików znajdujących się w różnych lokalizacjach.

## Zobacz także

1. Dokumentacja modułu `fs` w Node.js (https://nodejs.org/api/fs.html)
2. Dokumentacja modułu `path` w Node.js (https://nodejs.org/api/path.html)
3. Dokumentacja biblioteki `lodash` (https://lodash.com/docs)