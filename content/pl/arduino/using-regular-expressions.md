---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "Arduino: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wyrażenia regularne to jeden ze składników programowania Arduino, które pozwalają programistom dopasować, szukać, a nawet zastępować konkretne ciągi znaków. Programiści korzystają z nich, aby skrócić i uprzystępnić ikod oraz szybko przeszukiwać dane.

## Jak to Zrobić:
Tworzenie kodu z wyrażeniami regularnymi na Arduino jest proste. Zobacz poniższy przykład:

```Arduino 
#include <regex.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  regex_t regex;
  int reti;
  char msgbuf[100];

  reti = regcomp(&regex, "^a[[:alnum:]]", 0);
  if(reti){ Serial.println("Could not compile regex"); }

  reti = regexec(&regex, "abc", 0, NULL, 0);
  if(!reti){
    Serial.println("Match");
  }else if(reti == REG_NOMATCH){
    Serial.println("No match");
  }else{
    regerror(reti, &regex, msgbuf, sizeof(msgbuf));
    Serial.println("Regex match failed");
  }

  regfree(&regex);
}
```
Gdy uruchomisz ten kod, otrzymasz komunikat "Match", ponieważ ciąg "abc" pasuje do wyrażenia regularnego "^a[[:alnum:]]".

## Głębszy Wgląd:
Pojęcie wyrażenia regularnego powstało w teorii automatów i języków formalnych, a swoje początki bierze od matematyka Stephena Kleene'a w latach 50. XX wieku. Alternatywą dla wyrażenia regularnego w Arduino może być ręczne przeszukiwanie ciągów znaków, ale jest to proces mniej efektywny i trudniejszy w zarządzaniu. Co więcej, Arduino obsługuje kompilację wyrażeń regularnych przez procedury C, dlatego potrzebujemy dołączyć bibliotekę  `<regex.h>`.

## Zobacz także:
* Świetne źródło do nauki wyrażeń regularnych: https://regexone.com/ 
* Oryginalna praca Stephena Kleene'a: https://www.jstor.org/stable/1968861
* Dokumentacja Arduino Regex: https://www.arduino.cc/reference/en/