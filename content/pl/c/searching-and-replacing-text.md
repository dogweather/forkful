---
title:    "C: Wyszukiwanie i podmiana tekstu"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy w codziennej pracy napotykamy tekst, który wymaga pewnych zmian. Problem z początku wydaje się prosty - zwykłe podmienienie określonych znaków lub słów. Jednak jeśli mamy do czynienia z długimi fragmentami tekstu lub wieloma plikami, ręczne dokonywanie zmian staje się bardzo czasochłonne i podatne na błędy. Dlatego warto poznać możliwości i metody automatycznego wyszukiwania i zamiany tekstu przy użyciu języka programowania C.

## Jak to zrobić

Przedstawimy teraz kilka przykładów kodu w języku C, które pozwolą na zrozumienie procesu wyszukiwania i zamiany tekstu. Wszystkie przykłady będą znajdować się w blokach kodu "```C ... ```", aby ułatwić czytelnikom zrozumienie i samodzielne wykorzystanie prezentowanych rozwiązań.

### Przykład 1: Proste zamienianie tekstu

Załóżmy, że mamy tekst zawierający zdanie "Dzień dobry", a naszym celem jest zamiana go na "Miłego dnia". W tym celu potrzebujemy wykorzystać funkcję `str_replace` z biblioteki <string.h>. Przykładowy kod będzie wyglądał następująco:

```C
#include <stdio.h>
#include <string.h>

int str_replace(char *str, char *from, char *to) {
  char *found = strstr(str, from);
  
  if (!found) 
      return 0; // gdy nie znaleziono żądanego fragmentu

  int len_from = strlen(from);
  int len_to = strlen(to);
  int len_diff = len_to - len_from;
  int len_new = strlen(str) + len_diff + 1;

  char *tmp = malloc(len_new * sizeof(char));
  bzero(tmp, len_new);
  
  memcpy(tmp, str, found - str);
  memcpy(tmp + (found - str), to, len_to); // wstawienie nowego tekstu
  strcpy(tmp + (found - str) + len_to, found + len_from); // skopiowanie reszty tekstu

  strcpy(str, tmp);
  free(tmp);

  return 1; // sukces!
}

int main() {
  char str[] = "Dzień dobry";
  int result = str_replace(str, "Dzień", "Miłego dnia");
  
  if (result) 
      printf("%s\n", str);
  else 
      printf("Nie udało się zamienić tekstu\n");
  
  return 0;
}
```

Po wykonaniu tego kodu w konsoli pojawi się napis "Miłego dnia", zgodnie z naszym zamiarem.

### Przykład 2: Wyszukiwanie i zamiana w pliku tekstowym

Czasem może być wygodniej dokonać wyszukiwania i zamiany tekstu bezpośrednio w pliku tekstowym. Przykład ten wymaga użycia funkcji `fgetpos` i `fsetpos` z biblioteki <stdio.h>. W tym przypadku, dla celów demonstracyjnych, przykładowy plik tekstowy będzie nazywał się "input.txt" i miał już z góry wpisaną linię tekstu.

```C
#include <stdio.h>

int main() {
  FILE *fp = fopen("input.txt", "r+");
  char from[] = "Dzień dobry";
  char to[] = "Miłego dnia";
  
  char str[100];
  while (fgets(str, 100, fp) != NULL) {
    fpos_t pos;
    fgetpos(fp, &pos);

    char *found = strstr(str, from);
    if (found) {
      int len_from = strlen(from);
      int len_to = strlen(to);
      int len_diff = len_to - len_from;
      int len_new = strlen(str) + len_diff + 1;

      char *tmp = malloc(len_new * sizeof(char));
      bzero(tmp, len_new);
      memcpy(tmp, str, found -