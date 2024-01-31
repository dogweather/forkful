---
title:                "Edycja plików w miejscu przy użyciu jednolinijkowców CLI"
date:                  2024-01-27T16:21:39.062889-07:00
model:                 gpt-4-0125-preview
simple_title:         "Edycja plików w miejscu przy użyciu jednolinijkowców CLI"

category:             "Bash"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wyobraź sobie, że właśnie dowiedziałeś się, że musisz przeprowadzić masową aktualizację kilku plików konfiguracyjnych znajdujących się na twoim serwerze. Możesz otworzyć każdy plik, wprowadzić zmiany ręcznie i je zapisać. Albo możesz wykonać edycję na miejscu bezpośrednio z linii poleceń (CLI), co pozwala zaoszczędzić czas, zredukować ryzyko błędu i zautomatyzować powtarzalne zadania. Ta technika jest szczególnie przydatna do systemowych aktualizacji, poprawek lub modyfikacji zbiorowych, gdzie ręczne edytowanie mogłoby być niepraktyczne lub podatne na błędy.

## Jak to zrobić:

Jeśli chodzi o edycję plików na miejscu przy użyciu Bash, dwa wybitne narzędzia wchodzą w grę: `sed` i `awk`. Przyjrzyjmy się, jak używać tych potężnych narzędzi, na kilku przykładach kodu.

### Używanie `sed` do prostej zamiany tekstu

Poniższe polecenie zastępuje pierwsze wystąpienie "text1" na "text2" w `file.txt`:

```Bash
sed -i 's/text1/text2/' file.txt
```

Dla globalnej zamiany (wszystkie wystąpienia) dodałbyś na końcu `g`:

```Bash
sed -i 's/text1/text2/g' file.txt
```

Aby zmodyfikować wiele plików naraz:

```Bash
sed -i 's/text1/text2/g' file1.txt file2.txt file3.txt
```

### Używanie `awk` do bardziej złożonych manipulacji

`awk` to inne narzędzie, które wyróżnia się swoimi możliwościami programowania, szczególnie przydatnych przy przetwarzaniu tekstu, które wymaga manipulacji danymi opartymi na polach.

Zmiana drugiego pola każdej linii na `newValue` w `data.csv`, rozdzielanych przecinkami:

```Bash
awk -i inplace -F, '{$2="newValue"; print $0}' OFS=, data.csv
```

### Zrób kopię zapasową zanim zrobisz skok

Praktyczna rada: zawsze twórz kopię zapasową przed edycją na miejscu. `sed` ułatwia to za pomocą opcji `-i` po której następuje sufiks, aby utworzyć kopię zapasową.

```Bash
sed -i.bak 's/text1/text2/g' file.txt
```

To polecenie tworzy kopię zapasową oryginalnego `file.txt` jako `file.txt.bak` przed wykonaniem zamiany.

## Dogłębnie

Możliwość edytowania plików bezpośrednio z linii poleceń pojawiła się jako naturalny rozwój filozofii systemu Unix: dając użytkownikom możliwość efektywnego zarządzania i manipulowania danymi z jak najmniejszą liczbą klawiszy. Mimo to, ta moc niesie za sobą swoje pułapki.

### Kontekst historyczny

Narzędzia Uniksa takie jak `sed` i `awk` istnieją od wczesnych dni Unix, stworzone jako część filozofii zestawu narzędzi, koncentrującej się na specjalistycznych, komponowalnych poleceniach. Ich włączenie do arsenału Uniksa było odpowiedzią na potrzebę efektywnego przetwarzania tekstu w krajobrazie zdominowanym przez interfejsy linii poleceń.

### Alternatywy

Chociaż `sed` i `awk` są potężne, nie są jedynymi opcjami. Perl i Python, na przykład, mają opcje linii poleceń (`-p` i `-i` odpowiednio), które pozwalają na podobne możliwości edycji na miejscu, z możliwie bardziej zrozumiałą składnią dla złożonych operacji.

```Bash
perl -pi -e 's/text1/text2/g' file.txt
```

```Bash
python -c "import fileinput, sys; [sys.stdout.write(line.replace('text1', 'text2')) for line in fileinput.input(files='file.txt', inplace=True)]"
```

Każda alternatywa ma swoje mocne strony: ogromne możliwości jednolinijkowców Perla i syntax Pythona jest być może bardziej dostępny dla tych, którzy nie są głęboko zaznajomieni z narzędziami do przetwarzania tekstu w Uniksie.

### Szczegóły implementacji

Edycja na miejscu nie jest „na miejscu” w technicznym sensie. Zarówno `sed -i` jak i `awk -i inplace` działają przez utworzenie tymczasowego pliku, w którym przechowywane jest przetworzone wyjście, zanim zastąpi oryginalny plik. Ta metoda gwarantuje, że plik nie zostanie uszkodzony, jeśli proces zostanie przerwany. Implikacje dotyczą głównie zasobów i uprawnień: musisz mieć wystarczającą ilość miejsca na dysku dla tymczasowego pliku oraz uprawnienia do tworzenia plików w katalogu docelowego pliku.

Chociaż potężne, polecenia do edycji na miejscu muszą być używane z ostrożnością. Niewłaściwie umieszczone wyrażenie regularne może spowodować utratę danych, co podkreśla znaczenie tworzenia kopii zapasowych. Pomimo potencjalnych pułapek, opanowanie tych poleceń może znacznie zwiększyć twoją zdolność do szybkiego, efektywnego modyfikowania plików bezpośrednio z linii poleceń, ucieleśniając filozofię Unix'a polegającą na wykorzystywaniu prostych, potężnych narzędzi do realizacji złożonych zadań.
