---
title:                "C++: Wydrukowanie wyników debugowania"
simple_title:         "Wydrukowanie wyników debugowania"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Drukowanie wiadomości debugowania jest niezwykle przydatną umiejętnością w świecie programowania. Pozwala ono na wyświetlanie informacji o przebiegu działania programu, co znacznie ułatwia jego debugowanie oraz znajdowanie potencjalnych błędów. Jest to niezbędne w procesie tworzenia oprogramowania, dlatego warto nauczyć się tej techniki.

## Jak to zrobić

Aby wyświetlać wiadomości debugowania w C++, można skorzystać z funkcji `cout` z biblioteki `iostream`. Przykładem takiego użycia może być wyświetlanie wartości zmiennych w celu sprawdzenia ich poprawności. Poniżej znajduje się przykładowy kod z wykorzystaniem funkcji `cout`:

```
#include <iostream>

int main(){
    int liczba = 7;
    std::cout << "Wartość zmiennej liczba wynosi: " << liczba << std::endl;
    return 0;
}
```

Po skompilowaniu i uruchomieniu powyższego kodu, w konsoli zostanie wyświetlona wiadomość "Wartość zmiennej liczba wynosi: 7". Dzięki temu łatwo można kontrolować wartości zmiennych w trakcie wykonywania programu.

## Głębszy wgląd

Drukowanie wiadomości debugowania może być również użyteczne w przypadku łapania wyjątków. Można wtedy wyświetlić informacje o błędzie, który jest powodem wywołania wyjątku. W ten sposób łatwiej jest znaleźć przyczynę występowania błędów i je naprawić.

Innym sposobem na wyświetlanie informacji debugowania jest użycie funkcji `cerr`. Różni się ona od funkcji `cout` tym, że wiadomości zostaną wyświetlone w konsoli, nawet jeśli wystąpi błąd w programie. Dzięki temu można wyświetlić informacje o błędzie nawet gdy program się wykonał błędnie i zakończył działanie.

## Zobacz także

- [10 porad na łatwiejsze debugowanie kodu w C++](https://itcraftsman.pl/poradnik-na-latwiejsze-debugowanie-kodu-w-c/)
- [Wiadomości debugowania w Visual Studio](https://docs.microsoft.com/pl-pl/visualstudio/debugger/how-to-set-a-breakpoint-within-source-code?view=vs-2019)
- [Różnice między funkcjami cout i cerr w C++](https://www.geeksforgeeks.org/differences-between-printf-and-stdout-in-c/)