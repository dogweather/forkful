---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Rozpoczęcie nowego projektu to wynowanie nowej przestrzeni do tworzenia i doświadczenia kodowania od zera. Programiści to robią, aby postawić na nogi nowe idee, rozwiązać problemy lub nauczyć się nowych technologii.

## Jak to zrobić:

Tworzenie nowego projektu w JavaScript nie musi być trudne. Oto prosty przykład.

```Javascript
// Tworzenie nowego folderu dla Twojego projektu
mkdir myNewProject
cd myNewProject

// Inicjalizacja nowego projektu Node.js
npm init -y

// Tworzenie i edycja pliku HelloWorld.js
touch HelloWorld.js
code HelloWorld.js
```
Gdy to zrobisz, twój plik HelloWorld.js powinien otworzyć się w twoim wybranym edytorze kodu. Oto jak możemy dodać prosty kod.

```Javascript
console.log('Hello, world!');
```
Gdy to uruchomisz, output powinien wyglądać tak:

`Hello, world!`

## Głębokie Zanurzenie

Rozpoczęcie nowego projektu jest tradycją od początków programowania. Jego korzenie sięgają starożytnych matematyków, którzy zawsze zaczynali od czystej kartki.

Alternatywą dla ręcznego tworzenia nowych projektów jest użycie generatorów projektów, takich jak Yeoman. Te narzędzia automatycznie tworzą szkielety projektów, eliminując wiele powtarzalnych zadań.

Rozpoczynanie nowego projektu zawsze wiąże się z kilkoma szczegółami technicznymi. Na przykład, musisz zdecydować o strukturze folderów, biblotekach zależnych, stylu kodowania itp.

## Zobacz Również

1. [Node.js](https://nodejs.org/)
2. [npm - Node Package Manager](https://www.npmjs.com/)
3. [Yeoman - Generator Projektów](http://yeoman.io/)