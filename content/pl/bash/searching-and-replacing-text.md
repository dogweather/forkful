---
title:    "Bash: Wyszukiwanie i zamienianie tekstu"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub po prostu często pracujesz z plikami tekstowymi, prawdopodobnie musisz dokonywać zmian w tekście wielokrotnie. W takim przypadku bardzo przydatne jest narzędzie do wyszukiwania i zamiany tekstu. W Bashu istnieje kilka sposobów na przeprowadzenie tej operacji, które są niesamowicie proste i skuteczne. W tym artykule przyjrzymy się, dlaczego warto poznać te możliwości oraz jak je wykorzystać.

## Jak to zrobić

Pierwszym sposobem na wyszukiwanie i zamianę tekstu jest użycie polecenia `sed`. Przykładowo, jeśli chcemy zmienić wszystkie wystąpienia słowa "kot" na "pies" w pliku `animals.txt`, wykonujemy następującą komendę:
```
sed -i 's/kot/pies/g' animals.txt
```
Poleceń `sed` można używać do bardziej złożonych operacji, na przykład zmieniania wyrażeń regularnych. W poniższym przykładzie zmieniamy datę zapisaną w formacie DD/MM/YYYY na format YYYY-MM-DD:
```
sed -i 's/\([0-9]\{2\}\)\/\([0-9]\{2\}\)\/\([0-9]\{4\}\)/\3-\2-\1/g' data.txt
```
Dzięki użyciu flagi `-i` zmieniamy plik bezpośrednio, bez tworzenia nowego. Jeśli chcemy przeprowadzić operację na kilku plikach naraz, możemy zastosować polecenie `find` i przekazać je jako argument do `sed`:
```
find . -name "*.txt" -exec sed -i 's/kot/pies/g' {} +
```

Kolejną opcją jest użycie polecenia `awk` w połączeniu z poleceniem `sub`. W tym przypadku podajemy szukane wyrażenie, nowe wyrażenie oraz plik, na którym chcemy przeprowadzić operację. Należy pamiętać, że `awk` jest wrażliwy na wielkość liter:
```
awk '{sub(/kot/,"pies",$0); print}' animals.txt
```
Jeśli chcemy zastąpić wszystkie wystąpienia wyrażenia, możemy użyć dodatkowego parametru `g`:
```
awk '{sub(/kot/,"pies",$0);print}' animals.txt
```

## Głębszy wgląd

`sed` i `awk` to nie jedyne możliwości, jeśli chodzi o wyszukiwanie i zamianę tekstu w Bashu. Warto również wspomnieć o tym, że narzędzia te działają na strumieniach danych, co oznacza, że możemy je wykorzystać w połączeniu z innymi poleceniami.

Na przykład, jeśli chcemy wyświetlić tylko pliki z rozszerzeniem `.txt` i jednocześnie zamienić w nich wyrażenie "kot" na "pies", możemy użyć polecenia `grep` w połączeniu z `sed`:
```
grep -l "kot" *.txt | xargs -I {} sed -i 's/kot/pies/g' {}
```
Polecenie `grep -l` wyświetla tylko nazwy plików, zawierających podane wyrażenie. Następnie korzystamy z `xargs` aby przekazać te pliki jako argumenty do polecenia `sed`.

W podobny sposób można wykorzystać inne polecenia, takie jak `find` czy `xargs`, aby przeprowadzić operacje na wielu plikach naraz.

## Zobacz również

- [10 przydatnych poleceń Bash](https://www.linode.com/docs/tools-reference/tools/10-useful-bash-commands/)
- [Spraw