---
title:    "Bash: Konwertowanie daty na ciąg znaków"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest ważną umiejętnością w programowaniu Bash. Pozwala ona na wyświetlanie daty w bardziej czytelnej formie oraz na jej wykorzystanie w dalszych obliczeniach. W tym artykule dowiesz się, dlaczego warto nauczyć się tej umiejętności.

## Jak To Zrobić

Aby przekonwertować datę na ciąg znaków w Bash, możemy użyć polecenia `date`. Przykładowy kod wyglądałby następująco:

```Bash
date +"%d/%m/%Y"
```

Polecenie `date` z opcją `+%d/%m/%Y` spowoduje, że data zostanie przekonwertowana na format dzienne/miesięczne/roczne. W miejsce `/` można również użyć innych separatów, na przykład `-` lub `.`. Poniżej znajduje się lista dostępnych opcji:

- `%d` - dzień
- `%m` - miesiąc
- `%Y` - rok
- `%H` - godzina
- `%M` - minuta
- `%S` - sekunda
- `%A` - dzień tygodnia
- `%B` - nazwa miesiąca

### Przykład

Jeśli wykonamy powyższą komendę w terminalu, otrzymamy na przykład:

```
18/02/2021
```

Możemy również wybrać tylko część daty, którą chcemy wyświetlić. Na przykład, jeśli chcemy wyświetlić tylko rok oraz dzień tygodnia, możemy użyć polecenia:

```Bash
date +"%Y %A"
```

Co da nam wynik:

```
2021 Thursday
```

## Deep Dive

Konwersja daty na ciąg znaków może być wykorzystana w wielu różnych sposobach. Najczęściej służy do formatowania i wyświetlania daty w czytelny sposób, na przykład na stronach internetowych lub w raportach. Można również wykorzystać ją w skryptach do dalszych obliczeń. Ponadto, opcje `date` pozwalają na konwersję czasu GMT na lokalny oraz na odliczanie czasu.

## Zobacz Również

- [Dokumentacja polecenia `date` (ang.)](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Wideo o konwersji daty na ciąg znaków w Bash (pol.)](https://www.youtube.com/watch?v=xfSkCgWvmlI)