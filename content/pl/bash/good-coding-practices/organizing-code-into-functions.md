---
date: 2024-01-26 01:08:48.444438-07:00
description: "Jak to zrobi\u0107: Utw\xF3rz prost\u0105 funkcj\u0119 w Bashu."
lastmod: '2024-03-13T22:44:35.590754-06:00'
model: gpt-4-1106-preview
summary: "Utw\xF3rz prost\u0105 funkcj\u0119 w Bashu."
title: Organizacja kodu w funkcje
weight: 18
---

## Jak to zrobić:
Utwórz prostą funkcję w Bashu:

```Bash
greet() {
  echo "Cześć, $1!"
}
```

Użyj jej, wywołując funkcję z parametrem:

```Bash
greet "Świecie"  # Wyjście: Cześć, Świecie!
```

Funkcje mogą zwracać wartości za pomocą `return` dla numerycznych kodów statusu (nie do faktycznego zwracania danych):

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # Wyjście: 7
```

Zwróć uwagę, że `$?` przechwytuje wartość zwróconą przez ostatnie polecenie, którą jest numeryczny wynik funkcji `add`.

## Szczegółowe zagłębienie
W Bashu, funkcje służą do kompartmentalizacji kodu od wczesnych wersji. Historycznie, używanie funkcji jest zgodne z zasadami programowania strukturalnego wprowadzonymi w latach 60. aby poprawić jakość kodu.

Alternatywami dla funkcji mogą być dołączanie plików skryptów lub używanie aliasów, ale nie oferują one tego samego poziomu modularności i możliwości ponownego wykorzystania.

Godnym uwagi szczegółem implementacji w Bashu jest to, że funkcje są obywatelami pierwszej klasy; nie mają specjalnego słowa kluczowego deklaracji takiego jak `function` w innych językach, chociaż użycie `function` w Bashu jest opcjonalne dla czytelności. Zasięg funkcji również jest ciekawy – zmienne są domyślnie globalne, chyba że zostaną zadeklarowane jako lokalne, co może prowadzić do nieoczekiwanego zachowania, jeśli nie zostanie to odpowiednio zarządzane.

## Zobacz również
- Podręcznik Bash na temat Funkcji Powłoki: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Zaawansowany przewodnik po skryptach w Bashu: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" dla pogłębienia wiedzy na temat konceptów i praktyk skryptowania funkcji.
