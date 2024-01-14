---
title:    "Javascript: Pisanie pliku tekstowego"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego pisać plik tekstowy

Pisanie pliku tekstowego jest nieodłączną częścią programowania w języku JavaScript, ponieważ pozwala na przechowywanie i przetwarzanie danych w przejrzystej i zrozumiałej formie. Jest to szczególnie przydatne, gdy chcemy przechowywać duże ilości danych lub je udostępnić innym użytkownikom. Pisanie pliku tekstowego jest również niezbędne, gdy pracujemy z aplikacjami, które wymagają określonego formatu danych.

## Jak pisać plik tekstowy

Pisanie pliku tekstowego w języku JavaScript jest proste, ponieważ możemy skorzystać z wbudowanych funkcji i metod. W poniższym przykładzie pokażemy, jak stworzyć nowy plik tekstowy o nazwie "dane.txt" i dodać do niego kilka linii tekstu.

```Javascript
// Importujemy moduł fs, który pozwala nam na pracę z plikami
const fs = require('fs');

// Tworzymy nowy plik tekstowy o nazwie "dane.txt" i przypisujemy go do zmiennej
const plik = fs.createWriteStream('dane.txt');

// Dodajemy kilka linii tekstu do pliku, używając funkcji write()
plik.write('To jest tekstowy plik dane.txt\n');
plik.write('Zapisz te informacje dla późniejszego użytkownika\n');

// Zamykamy plik, używając metody end()
plik.end();
```

Po uruchomieniu tego kodu, w katalogu projektu powinien pojawić się plik "dane.txt" zawierający podany tekst. Jeśli chcemy odczytać dane z pliku tekstowego, możemy użyć funkcji readFileSync(), która zwróci zawartość pliku jako ciąg znaków.

```Javascript
// Odczytujemy plik dane.txt i zapisujemy jego zawartość do zmiennej
const zawartosc = fs.readFileSync('dane.txt', 'utf8');

// Wyświetlamy zawartość pliku w konsoli
console.log(zawartosc); 
```

## Głębszy wgląd

Pisanie plików tekstowych może wymagać dodatkowych ustawień, takich jak określenie kodowania znaków lub trybu zapisu. Możemy to zrobić, przekazując odpowiednie opcje do funkcji createWriteStream() lub readFileSync(). Istnieje również wiele innych metod z modułu fs, które pomogą nam w manipulowaniu plikami, takich jak rename() czy appendFile(). Ważne jest jednak, aby pamiętać o obsłudze wyjątków i błędów, które mogą wystąpić podczas pisania lub odczytywania plików.

## Zobacz także

- [Dokumentacja modułu fs w języku JavaScript](https://nodejs.org/api/fs.html)
- [Tutorial: Pisanie i odczytywanie plików w języku JavaScript](https://www.digitalocean.com/community/tutorials/nodejs-reading-and-writing-files)
- [Wideo: Tworzenie i zapisywanie plików w języku JavaScript](https://www.youtube.com/watch?v=EnaQd-6hQZU)