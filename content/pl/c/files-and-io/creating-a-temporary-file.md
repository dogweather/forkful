---
aliases:
- /pl/c/creating-a-temporary-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:19.951180-07:00
description: "Tworzenie tymczasowego pliku w j\u0119zyku C polega na generowaniu pliku,\
  \ kt\xF3ry ma by\u0107 u\u017Cywany przez kr\xF3tki czas, zazwyczaj jako przestrze\u0144\
  \ robocza dla\u2026"
lastmod: 2024-02-18 23:08:50.101023
model: gpt-4-0125-preview
summary: "Tworzenie tymczasowego pliku w j\u0119zyku C polega na generowaniu pliku,\
  \ kt\xF3ry ma by\u0107 u\u017Cywany przez kr\xF3tki czas, zazwyczaj jako przestrze\u0144\
  \ robocza dla\u2026"
title: Tworzenie tymczasowego pliku
---

{{< edit_this_page >}}

## Co i dlaczego?
Tworzenie tymczasowego pliku w języku C polega na generowaniu pliku, który ma być używany przez krótki czas, zazwyczaj jako przestrzeń robocza dla przetwarzania danych lub ich przechowywania. Programiści robią to, aby zarządzać tymczasowymi danymi bez wpływania na stałą pamięć programu lub aby zapewnić, że wrażliwe dane zostaną usunięte po użyciu.

## Jak to zrobić:
Tworzenie tymczasowego pliku w języku programowania C może wykorzystywać funkcje takie jak `tmpfile()` i `mkstemp()`.

**Korzystanie z `tmpfile()`**: Ta funkcja tworzy unikalny tymczasowy plik, który jest automatycznie usuwany, gdy program zostanie zakończony lub plik zostanie zamknięty.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Nie udało się utworzyć pliku tymczasowego");
        return 1;
    }

    // Zapisywanie danych do pliku tymczasowego
    fputs("To jest test.\n", temp);

    // Powrót i odczyt tego, co zapisaliśmy
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // Automatycznie usuwany przy zamknięciu lub wyjściu z programu
    fclose(temp);

    return 0;
}
```
**Przykładowe wyjście:**
```
To jest test.
```

**Korzystanie z `mkstemp()`**: Zapewnia większą kontrolę nad lokalizacją tymczasowego pliku i jego uprawnieniami. Wymaga ciągu szablonu zakończonego `XXXXXX`, który następnie zastępuje unikalną sekwencją, aby zapobiec kolizjom nazw.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char template[] = "/tmp/mytemp-XXXXXX";
    int fd = mkstemp(template);

    if (fd == -1) {
        perror("Nie udało się utworzyć pliku tymczasowego");
        return 1;
    }
    
    printf("Utworzono plik tymczasowy: %s\n", template);

    // Tymczasowe pliki utworzone za pomocą mkstemp() należy usuwać ręcznie
    unlink(template);

    close(fd);
    return 0;
}
```
**Przykładowe wyjście:**
```
Utworzono plik tymczasowy: /tmp/mytemp-abc123
```

## Pogłębiona analiza
Koncepcja plików tymczasowych nie jest unikalna dla C, ale jest powszechną funkcjonalnością w wielu środowiskach programistycznych ze względu na jej użyteczność w obsłudze danych efemerycznych. Funkcja `tmpfile()`, ustandaryzowana w standardzie ISO C, tworzy plik o unikalnej nazwie w standardowym katalogu, ale jego istnienie jest przelotne, co sprawia, że jest idealny do bezpiecznych lub tymczasowych operacji.

Jednym z istotnych ograniczeń `tmpfile()` jest jej zależność od domyślnego katalogu tymczasowego, który może nie być odpowiedni dla wszystkich aplikacji, szczególnie pod względem uprawnień lub bezpieczeństwa. W przeciwieństwie do tego, `mkstemp()` pozwala na określenie katalogu i zapewnia bezpieczne tworzenie plików z gwarantowaną unikalnością nazw poprzez modyfikację dostarczonego ciągu szablonu, oferując bardziej wszechstronne rozwiązanie kosztem ręcznego zarządzania plikami.

Jednak tworzenie plików tymczasowych może wprowadzać podatności bezpieczeństwa, takie jak warunki rywalizacji, jeśli nie są one odpowiednio obsługiwane. Na przykład, `tmpfile()` i `mkstemp()` adresują różne aspekty bezpiecznego tworzenia tymczasowych plików (automatyczne usuwanie i bezpieczne generowanie nazw, odpowiednio), ale żadna z nich nie jest panaceum. Programiści muszą rozważyć specyfikę potrzeb bezpieczeństwa swojej aplikacji, włączając w to potencjalne podatności wprowadzane przez pliki tymczasowe, i mogą potrzebować wdrożyć dodatkowe zabezpieczenia poza tym, co oferują te funkcje.

W szerszym kontekście programowania, alternatywy takie jak przechowywanie danych w pamięci (np. przy użyciu dynamicznych struktur danych lub plików zmapowanych w pamięci) mogą oferować lepszą wydajność lub bezpieczeństwo dla tymczasowego przetwarzania danych. Niemniej, fizyczne pliki tymczasowe pozostają kluczowym narzędziem w wielu scenariuszach, szczególnie przy dużych zestawach danych lub gdy zaangażowana jest komunikacja międzyprocesowa.
