---
title:                "Fish Shell: Konwertowanie ciągu znaków na małe litery"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

### Dlaczego?

Jeśli jesteś programistką lub programistą, prawdopodobnie spotkałaś się z sytuacją, w której potrzebowałaś lub potrzebowałeś zamienić napis na małe litery. Czy to dla porównania ze wzorcem, czy dla ujednolicenia danych, konwersja ta jest niezbędna w wielu przypadkach. W tym wpisie dowiecie się, jak zrobić to w Fish Shell.

### Jak to zrobić?

Możesz użyć wbudowanej funkcji `string tolower`, która konwertuje napis na małe litery. Spróbujmy to na praktycznym przykładzie:

```Fish Shell
string tolower "PRZYKŁADOWY NAPIS"
```

Wynik wywołania tej funkcji będzie po prostu "przykładowy napis". Zauważmy, że funkcja ta jest nieczuła na to, czy w podanym napisie są już małe litery czy nie. Pozwala to na łatwe porównywanie napisów bez konieczności uwzględniania wielkości liter.

Jeśli jednak chcemy mieć pełną kontrolę nad konwersją, możemy użyć innych wbudowanych funkcji jak `string tolower-macron` lub `string tolower-accent`. Warto też zapoznać się z funkcją `string toupper`, która konwertuje napis na duże litery.

### Dogłębny wgląd

W Fish Shell konwersja napisów oparta jest na standardowej bibliotece C dla języka C. Oznacza to, że funkcje te wykonują pewne operacje na poszczególnych znakach w napisie, np. zmiana wartości kodu ASCII na odpowiednią wartość dla małych liter. W przypadku użycia specjalnych znaków lub liter z akcentami, konwersja może być bardziej skomplikowana, dlatego też istnieją funkcje `tolower-macron` i `tolower-accent`, które obsługują te przypadki.

### Zobacz także

- [Dokumentacja Fish Shell - String Manipulation](https://fishshell.com/docs/current/tutorial.html#tutorial-manip)
- [Dokumentacja Fish Shell - String Functions](https://fishshell.com/docs/current/cmds/string.html)
- [Poradnik programisty - Konwersja napisów na małe litery w Shell](https://www.shell-tips.com/2014/06/04/how-to-convert-uppercase-letters-to-lowercase-in-bash/)