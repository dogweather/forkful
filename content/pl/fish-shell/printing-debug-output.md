---
title:                "Drukowanie wyników debugowania"
html_title:           "Fish Shell: Drukowanie wyników debugowania"
simple_title:         "Drukowanie wyników debugowania"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co to jest debugowanie i dlaczego programiści to robią? 
Debugowanie polega na wyświetlaniu informacji o działaniu programu w celu zidentyfikowania i naprawy ewentualnych błędów. Programiści używają tego narzędzia, aby ułatwić sobie proces tworzenia i naprawiania kodu oraz uniknąć potencjalnych problemów podczas działania programu.

## Jak tego dokonać w Fish Shell?
W Fish Shell możemy wyświetlać debug output używając polecenia "echo". Przykładowy kod i jego output przedstawiony jest poniżej:

```
echo "To jest przykładowy debug output."
```

Output:
```
To jest przykładowy debug output.
```

Możemy również użyć specjalnego flaga "-d" w poleceniu "set" do wyświetlania debug outputu dla konkretnych zmiennych. Przykładowy kod i output:

```
set -d zmienna "To jest przykładowy debug output."
echo $zmienna
```

Output:
```
To jest przykładowy debug output.
```

## Głębsze informacje
Debugowanie jest ważnym elementem procesu programowania od samego początku istnienia informatyki. Alternatywnymi narzędziami do wyświetlania debug outputu w Fish Shell są również polecenia "printf" i "printf '%s\n'" oraz funkcje "debug" i "debug_msg". W Fish Shell istnieje również możliwość włączenia całego trybu debugowania używając flagi "-d" przy uruchamianiu shella lub dodając ją w pliku konfiguracyjnym Fish Shell (config.fish).

## Zobacz również
- Dokumentacja Fish Shell:
https://fishshell.com/docs/current/
- Tutorial o debugowaniu w Fish Shell:
https://fishshell.com/docs/current/commands.html#debugging
- Przykładowy kod z wykorzystaniem debug outputu:
https://github.com/fingercom/fish-shell-examples/blob/master/debug.fish