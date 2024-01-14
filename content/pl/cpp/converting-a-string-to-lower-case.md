---
title:    "C++: Konwertowanie ciągu znaków na małe litery."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

C++ jest jednym z najbardziej popularnych języków programowania, wykorzystywanym w wielu dziedzinach, takich jak tworzenie gier, aplikacji desktopowych i systemów wbudowanych. Jedną z podstawowych operacji, które możemy wykonać na ciągach znaków (stringach) w C++, jest konwersja ich do małych liter. W tym wpisie pokażemy dlaczego warto nauczyć się tej operacji oraz jak ją wykonać.

## Jak to zrobić

Konwersja ciągu znaków do małych liter może być przydatna w wielu sytuacjach, na przykład gdy użytkownik wprowadza dane do programu i nie chcemy mieć problemów ze znakami wielkich liter. W celu zmniejszenia ryzyka błędów i uproszczenia operacji na ciągach znaków, możemy skorzystać z wbudowanej funkcji C++ - `tolower()`. Poniżej przedstawione są dwa przykłady użycia tej funkcji:

```C++
// Przykład 1 - Konwersja całego ciągu znaków do małych liter
#include <iostream>
#include <string>

using namespace std;

int main() {
    string tekst = "PRZYKŁADOWY TEKST";
    cout << "Oryginalny tekst: " << tekst << endl;
    
    for (int i = 0; i < tekst.size(); i++) {
        tekst[i] = tolower(tekst[i]);
    }
    
    cout << "Tekst po konwersji: " << tekst << endl;
    return 0;
}

// Przykład 2 - Konwersja konkretnego znaku w ciągu do małej litery
#include <iostream>
#include <string>

using namespace std;

int main() {
    string tekst = "Inny przykład";
    cout << "Oryginalny tekst: " << tekst << endl;
    
    // Konwersja drugiego znaku w ciągu
    tekst[1] = tolower(tekst[1]);
    
    cout << "Tekst po konwersji: " << tekst << endl;
    return 0;
}
```

Po uruchomieniu tych przykładów, na ekranie pojawi się następujący wynik:

```
Przykład 1:
Oryginalny tekst: PRZYKŁADOWY TEKST
Tekst po konwersji: przykładowy tekst

Przykład 2:
Oryginalny tekst: Inny przykład
Tekst po konwersji: inny przykład
```

W pierwszym przykładzie dokonujemy konwersji całego ciągu znaków, używając pętli `for` i funkcji `tolower()`, która jest dostępna w bibliotece `string`. W drugim przykładzie, konwertujemy tylko jeden konkretny znak w ciągu. Pamiętajmy, że numerowanie znaków w ciągu zaczyna się od zera.

## Głębsze spojrzenie

Operacja konwersji małych liter jest bardzo prosta i łatwa do zrozumienia, ale może być przydatna w wielu przypadkach, szczególnie przy pracy z użytkownikami i danymi wejściowymi. Pamiętajmy, że funkcja `tolower()` jest częścią biblioteki standardowej języka C++, co oznacza że jest dostępna dla wszystkich programistów C++. Warto także zwrócić uwagę, że w przypadku niektórych alfabetów, konwersja do małych liter może spowodować zmianę znaczenia danego słowa, dlatego należy zachować ostrożność przy jej wykorzystaniu.

## Zobacz także

- Dokumentacja C++ o funkcji `tolower()` - https://www.cplusplus.com/reference/cctype/tolower/ 
- Przykładowy kod z konwersją ciągu do małych liter - https://www.tutorialspoint.com/cplusplus-program-to-convert-a-string-to-lowercase