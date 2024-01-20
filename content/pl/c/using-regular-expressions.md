---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "Arduino: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Regularne wyrażenia to skuteczne narzędzie do przeszukiwania i podmieniania tekstów. Programiści używają ich przede wszystkim ze względu na ich potężne możliwości i efektywność.

## Jak to zrobić:
```C
#include <regex.h>

int main() {
   regex_t regex;
   char msgbuf[100];

   regcomp(&regex, "abc", 0);
   if (regexec(&regex, "abcdef", 0, NULL, 0) == 0) {
      printf("Dopasowano\n");
   } else {
      regerror(reti, &regex, msgbuf, sizeof(msgbuf));
      printf("Dopasowanie nie powiodło się: %s\n", msgbuf);
   }

   regfree(&regex);
   return 0;
}
```

Przykładowe wyjście:
```
Dopasowano
```

## Pogłębienie

Pierwsze regularne wyrażenia zostały opracowane w laboratoriach AT&T Bell w latach 50. i 60. XX wieku. Wprowadzenie ich do C dość długo trwało, ale ostatecznie znalazły swoje miejsce w bibliotece POSIX.

Alternatywą dla standardowych wyrażeń regularnych w C jest użycie bibliotek zewnętrznych, takich jak PCRE (Perl Compatible Regular Expressions).

Ważne jest, że mimo iż potentat w swojej domenie, regularne wyrażenia nie są panaceum na wszystko. Większość problemów "pasuje do" regularnych wyrażeń, ale niektóre są poza ich zasięgiem — na przykład parsing zagnieżdżonych struktur.

## Zobacz również
- [Manual GNU](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Regular Expressions in C](https://www.lemoda.net/c/unix-regex/) 
- [Tutorial on Regex in C](https://www.tutorialspoint.com/cprogramming/c_regular_expressions.htm)