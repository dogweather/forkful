---
title:    "Arduino: Odczytywanie argumentów wiersza poleceń"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak można wpływać na działanie programów, wprowadzając różne argumenty z linii poleceń? Jeśli jesteś zainteresowany dowiedzeniem się, jak łatwo można osiągnąć ten cel, ten artykuł jest dla Ciebie!

## Jak to zrobić

Aby odczytać argumenty z linii poleceń w Arduino, należy użyć funkcji ```Arduino.args()```, która zwraca tablicę z argumentami przekazanymi do programu. Następnie można wykorzystać pętlę ```for``` do przejrzenia każdego argumentu i wykonania odpowiednich działań. Oto prosty przykład kodu:

```
Arduino.args();
for(int i=0; i<Arduino.args().length; i++){
  Arduino.print("Argument " + i + ": " + Arduino.args()[i]);
}
```

Przy założeniu, że do programu przekazano 3 argumenty, kod ten wyświetli na monitorze szeregu argumentów wraz z ich numerami:

```
Argument 0: pierwszy
Argument 1: drugi
Argument 2: trzeci
```

Zauważ, że aby uzyskać dostęp do tablicy argumentów, musimy wywołać funkcję ```Arduino.args()``` więcej niż raz, ponieważ wartość ta może ulec zmianie podczas działania programu.

## Dogłębna analiza

Odczytanie argumentów z linii poleceń może być przydatne w wielu sytuacjach, na przykład przy wykorzystaniu flag konfiguracyjnych lub wywoływaniu różnych funkcji w zależności od przekazanych argumentów. Dzięki temu będziemy mieć większą kontrolę nad działaniem naszych programów i łatwiej będzie je dostosowywać do różnych potrzeb.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o funkcji ```Arduino.args()```, polecamy zapoznać się z oficjalną dokumentacją Arduino oraz przeczytać artykuł "Tutorial: Odczytywanie argumentów z linii poleceń w Arduino" na stronie [WiFi for Beginners](https://www.jclerc.com/wififorb.php).