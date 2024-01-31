---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"

category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
(W co grają testy?)
Piszesz kawałek kodu – piszesz test. Sprawdzasz, czy produkt działa. Dlatego: oszczędność czasu, jakość, aż po bezpieczeństwo.

## How to:
(Jak to ugryźć?)
Prosty przykład testu w C z użyciem frameworka `Assert.h`:

```C
#include <assert.h>
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

void test_add_function() {
    assert(add(2, 2) == 4);
    assert(add(-1, 1) == 0);
    printf("Wszystko działa!\n");
}

int main() {
    test_add_function();
    return 0;
}
```

Jakiego outputu się spodziewać? Oto on:

```
Wszystko działa!
```

## Deep Dive
(Pogłębiamy wiedzę)
Kiedyś: ręczne testowanie. Dziś: automatyzacja. Alternatywy: CUnit, Check, cmocka. Istotą jest mockowanie i asercje. Chodzi o symulację i weryfikację.

## See Also
(Zobacz też)
Dobre źródła to:
- [CUnit](http://cunit.sourceforge.net/)
- [Check](https://libcheck.github.io/check/)
- [cmocka](https://cmocka.org/)
- [Assert.h documentation](https://en.cppreference.com/w/c/error/assert)

Sprawdź je, dzięki temu twój kod będzie jeszcze lepszy.
