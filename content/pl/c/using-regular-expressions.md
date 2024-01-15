---
title:                "Wykorzystywanie wyrażeń regularnych"
html_title:           "C: Wykorzystywanie wyrażeń regularnych"
simple_title:         "Wykorzystywanie wyrażeń regularnych"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego: Dlaczego warto używać wyrażeń regularnych w C

Wyrażenia regularne są niezwykle pomocne przy przetwarzaniu tekstów i danych w programowaniu w C. Pozwalają nam na szybkie i precyzyjne wyszukiwanie oraz manipulację łańcuchami znaków.

## Jak to zrobić: Przykłady kodu

Oto kilka przykładów kodu, które pokazują jak używać wyrażeń regularnych w języku C.

```C
#include <stdio.h>
#include <string.h>
#include <regex.h>

int main() {
  char *string = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
  char *pattern = "ipsum.*amet";

  regex_t regex;
  regcomp(&regex, pattern, REG_EXTENDED);

  int match = regexec(&regex, string, 0, NULL, 0);
  if (match == 0) {
    printf("Pasuje!\n");
  } else if (match == REG_NOMATCH) {
    printf("Nie pasuje...\n");
  } else {
    char error_message[100];
    regerror(match, &regex, error_message, sizeof(error_message));
    printf("Błąd: %s\n", error_message);
  }

  regfree(&regex);
  return 0;
}
```

Ten kod wyszukuje w łańcuchu znaków "string" fragment pasujący do wyrażenia regularnego "pattern", w tym przypadku słowo "ipsum" z dowolną ilością dowolnych znaków po nim, a następnie słowo "amet". Jeśli jest dopasowanie, program wypisze "Pasuje!", w przeciwnym wypadku "Nie pasuje...". Można również użyć wyrażeń regularnych do zastępowania tekstu lub wyodrębniania konkretnych fragmentów.

```C
#include <stdio.h>
#include <string.h>
#include <regex.h>

int main() {
  char *string = "John Smith, 39 years old, j.smith@example.com";
  char *pattern = "([A-Z][a-z]+) ([A-Z][a-z]+), ([0-9]+) .* (.*)";

  regex_t regex;
  regcomp(&regex, pattern, REG_EXTENDED);

  regmatch_t groups[5];
  regexec(&regex, string, 5, groups, 0);

  char name[50];
  strncpy(name, string + groups[1].rm_so, groups[1].rm_eo - groups[1].rm_so);
  name[groups[1].rm_eo - groups[1].rm_so] = '\0';
  printf("Imię: %s\n", name);

  char surname[50];
  strncpy(surname, string + groups[2].rm_so, groups[2].rm_eo - groups[2].rm_so);
  surname[groups[2].rm_eo - groups[2].rm_so] = '\0';
  printf("Nazwisko: %s\n", surname);

  char age[50];
  strncpy(age, string + groups[3].rm_so, groups[3].rm_eo - groups[3].rm_so);
  age[groups[3].rm_eo - groups[3].rm_so] = '\0';
  printf("Wiek: %s\n", age);

  char email[50];
  strncpy(email, string + groups[4].rm_so, groups[4].rm_eo - groups[4].rm_so);
  email[groups[4].rm_eo - groups[4].rm_so] = '\0';
  printf("Email: %s\n", email);

  regfree(&regex);
  return 0;
}
```

Ten przykład pokazuje bardziej zaawansowane użycie wyrażeń regularnych w celu wyodrębnienia różnego rodzaju informacji z tekstu, korzystając z grup w wyrażeniu regularnym i funkcji "strncpy".

## Głębszy zanurzenie: Zastosowania i użycie zaawansowane

Wyrażenia regularne nie są tylko przydatne do wyszukiwania i manipulacji tekstu. W języku C można również wykorzystać je do sprawdzania poprawności danych, np. w formularzach lub plikach konfiguracyjnych.