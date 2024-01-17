---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Arduino: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyszukiwanie i zastępowanie tekstu jest procesem polegającym na wyszukiwaniu konkretnego ciągu znaków w tekście i jego zamianie na inny ciąg znaków. Programiści często wykonują tę czynność, aby zmienić pewne części kodu lub ujednolicić jego wygląd.

## Jak to zrobić:
```
char text[] = "Witaj świecie!";
char oldMsg[] = "Witaj";
char newMsg[] = "Cześć";
str_replace(text, oldMsg, newMsg);
Serial.println(text);
//Output: "Cześć świecie!"
```
W powyższym przykładzie mamy zmienną "text", w której znajduje się tekst "Witaj świecie!". Następnie wykorzystujemy funkcję "str_replace" (zdefiniowaną w bibliotece Arduino) w celu zmiany słowa "Witaj" na "Cześć". Ostatecznie na monitorze szeregowym wyświetlany jest zmieniony tekst "Cześć świecie!". W ten sposób możemy dokonywać zastępowania tekstu w dowolnym miejscu naszego kodu.

## Zagłębienie:
Wyszukiwanie i zastępowanie tekstu jest często wykorzystywane w programowaniu, szczególnie przy tworzeniu większych projektów z wieloma plikami i liniami kodu. Można również wykorzystać inne sposoby na dokonanie tej samej czynności, takie jak użycie wyrażeń regularnych. W implementacji funkcji "str_replace" wykorzystywany jest algorytm Knutha-Morrisa-Pratta, co pozwala na wydajne i szybkie wyszukiwanie i zastępowanie tekstu w dłuższych napisach.

## Zobacz także:
- [Kurs programowania Arduino dla początkujących](https://create.arduino.cc/projecthub/Arduino_Genuino/arduino-tutorial-for-beginners-1-113b44)
- [Dokumentacja funkcji "str_replace" w Arduino](https://www.arduino.cc/reference/en/language/functions/strings/strreplace/)