---
title:                "Drukowanie komunikatów debugowania"
date:                  2024-01-20T17:51:59.830377-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Debugowanie to nasz detektywistyczny proces mapowania problemów do rozwiązań. Printowanie outputu w trakcie debugowania pozwala na podglądanie stanu programu w danym punkcie – jak mały światło rzucający światło na nasze kodowe mroki.

## Jak to zrobić:
```C
#include <stdio.h>

int main() {
    int a = 5;
    int b = 10;
    int sum = a + b;
    // Poniżej prosty print debugujący
    printf("Debug: a=%d, b=%d, sum=%d\n", a, b, sum);
    
    //...kod kontynuuje
    
    return 0;
}
```
Wyjście:
```
Debug: a=5, b=10, sum=15
```

## Światło w mroku:
Już w latach 70., programiści używali prostych komunikatów, by zrozumieć, co się dzieje w ich programach. Dziś `printf` to klasyk, ale istnieją alternatywy jak np. `stderr` dla błędów, czy loggera z biblioteki `<syslog.h>` dla systemu Unix. Implementacja outputu debugowego ogólnie zależy od potrzeb: poziomy logowania, formaty, zapis do pliku… różnorodność jest spora, warto ją wykorzystać.

## Zobacz również:
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/
- Manual `printf`: https://en.cppreference.com/w/c/io/fprintf
- Logging w C: https://www.gnu.org/software/libc/manual/html_node/Logging-Messages.html
