---
title:                "Praca z json"
html_title:           "C: Praca z json"
simple_title:         "Praca z json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON (JavaScript Object Notation) jest popularnym formatem danych, szczególnie w aplikacjach internetowych. Jest on łatwy do zapisywania i przesyłania danych, a także łatwy do pracy z nim w C. Dzięki temu, że jest również czytelny dla ludzi, jest idealnym wyborem dla projektów, które wymagają przesyłania i przechowywania danych w formie tekstowej.

## Jak

Aby rozpocząć pracę z JSON w C, musimy najpierw zaimportować bibliotekę <stdio.h> oraz bibliotekę <stdlib.h>, która jest potrzebna do obsługi pamięci dynamicznej. Następnie możemy wykorzystać funkcję `fgetc()` do pobrania pojedynczego znaku ze standardowego wejścia. Następnie, używając pętli `while`, możemy czytać znak po znaku aż do końca pliku, a następnie przypisać odczytane znaki do bufora znakowego. W ten sposób utworzymy ciąg znaków, który następnie możemy przekonwertować na format JSON przy użyciu funkcji `json_parse()`.

Przykładowy kod można napisać w następujący sposób:

```
#include <stdio.h>
#include <stdlib.h>
#include <json-c/json.h>

int main()
{
    FILE *fptr;
    char filename[100], c;
    char *buffer;

    // Otwórz plik lub wczytaj dane z innego źródła

    fptr = fopen("data.json", "r");

    if (fptr == NULL)
    {
        printf("Nie można otworzyć pliku.");
        return 0;
    }

    // Alokuje pamięć dla bufora

    buffer = (char *)malloc(1000 * sizeof(char));

    if(buffer == NULL)
    {
        printf("Wystąpił błąd podczas alokacji pamięci.");
        return 0;
    }

    // Czytaj plik znak po znaku i przypisuj do bufora

    c = fgetc(fptr);
    int i = 0;

    while (c != EOF)
    {
        buffer[i] = c;
        i++;
        c = fgetc(fptr);
    }

    buffer[i] = '\0';

    // Konwertuj bufor do formatu JSON

    struct json_object *parsed_json;
    parsed_json = json_tokener_parse(buffer);

    // Wyświetl odczytane dane

    printf("Dane z pliku JSON:\n%s\n", json_object_to_json_string(parsed_json));

    fclose(fptr);
    free(buffer);
    return 0;
}
```

Przykładowy plik JSON (`data.json`):

```
{
    "imie": "Jan",
    "nazwisko": "Kowalski",
    "wiek": 30
}
```

Przykładowy wynik:

```
Dane z pliku JSON:
{"imie": "Jan", "nazwisko": "Kowalski", "wiek": 30}
```

## Deep Dive

Biblioteka json-c jest dostępna w systemach Unix oraz w systemie Windows. Bez problemu można ją również użyć w aplikacjach internetowych. Bezpośrednio po pobraniu znaków za pomocą funkcji `fgetc()` należy przekonwertować je na format JSON przy użyciu funkcji `json_tokener_parse()`. Dzięki temu zyskujemy dostęp do wielu narzędzi, takich jak `json_object_to_json_string()`, które są przydatne w dalszej pracy z danymi JSON.

## Zobacz również
- Dokumentacja biblioteki json-c: https://json-c.github.io/json-c/
- Inne biblioteki do pracy z JSON w C: https://github.com/miloyip/nativejson-benchmark/tree/master/src
- Przykładowe aplikacje wykorzystujące JSON w C: https://github.com/search?q=json+c&type=Repositories
- Poradnik na temat pracy z JSON w C: https://www.tutorialspoint.com/json/json_c_example.html