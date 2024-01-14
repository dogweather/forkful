---
title:                "Javascript: Zaczynając nowy projekt"
programming_language: "Javascript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego zacząć nowy projekt?

Nie ma nic bardziej satysfakcjonującego dla programisty niż zaczęcie nowego projektu. Jest to szansa na wykorzystanie wiedzy i umiejętności w praktyce oraz stworzenie czegoś nowego i użytecznego. Przeczytaj dalszą część, aby dowiedzieć się jak rozpocząć nowy projekt w języku Javascript.

## Jak to zrobić?

### Tworzenie podstawowego pliku HTML

Na początku potrzebujemy pustego pliku HTML, który będzie podstawą dla naszego projektu. W tym celu otwórz swoje ulubione IDE (np. Visual Studio Code) i utwórz plik o nazwie "index.html". Następnie wpisz poniższy kod:

```Javascript
<!DOCTYPE html>
<html lang="pl">
<head>
    <meta charset="UTF-8">
    <title>Mój projekt</title>
</head>
<body>
    <h1>Witaj, świecie!</h1>
</body>
</html>
```

### Dodawanie arkusza stylów CSS

Jeśli chcesz dodać trochę stylu do swojego projektu, możesz stworzyć nowy plik CSS o nazwie "style.css". Następnie w pliku "index.html" należy dodać odwołanie do tego pliku używając tagu ```<link>```, jak pokazano poniżej:

```Javascript
<!DOCTYPE html>
<html lang="pl">
<head>
    <meta charset="UTF-8">
    <title>Mój projekt</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <h1>Witaj, świecie!</h1>
</body>
</html>
```

### Dodawanie kodu Javascript

Jeśli chcesz dodać interaktywność do swojego projektu, możesz użyć języka Javascript. W pliku "index.html" możesz dodać tag ```<script>``` i umieścić w nim kod Javascript, jak poniżej:

```Javascript
<!DOCTYPE html>
<html lang="pl">
<head>
    <meta charset="UTF-8">
    <title>Mój projekt</title>
</head>
<body>
    <h1>Witaj, świecie!</h1>
    <script>
        let name = "Jan";
        console.log("Cześć, " + name + "!");
    </script>
</body>
</html>
```

### Podgląd wyniku

Aby zobaczyć efekt swojego kodu, możesz otworzyć plik "index.html" w przeglądarce internetowej. Powinna się otworzyć strona z napisem "Witaj, świecie!" oraz wyświetlić w konsoli przeglądarki wiadomość "Cześć, Jan!".

## Dogłębne wgląd

* Warto zacząć od zaplanowania swojego projektu i ustalenia celów, które chcesz osiągnąć.
* Niezbędne jest zrozumienie podstaw języka Javascript, takich jak zmienne, funkcje czy pętle.
* Wykorzystanie bibliotek i frameworków może znacznie ułatwić pracę z językiem Javascript.

## Zobacz także

* [Wprowadzenie do języka Javascript](https://developer.mozilla.org/pl/docs/Wprowadzenie_do_języka_JavaScript)
* [Początek z JavaScript](https://www.w3schools.com/js/)
* [10 kroków do rozpoczęcia nauki języka JavaScript](https://medium.com/@ibejalon/how-to-start-learning-javascript-in-10-basic-steps-bfa632f4c06)