---
title:    "Fish Shell: Usuwanie znaków pasujących do wzoru."
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista, który korzysta z powłoki Fish, może natknąć się na sytuację, w której chce usunąć znaki pasujące do wzoru. Może to być przydatne przy wykonywaniu operacji na plikach lub manipulacji tekstem.

## Jak to zrobić

Do usunięcia znaków pasujących do wzoru w shellu Fish możemy wykorzystać komendę `string replace` w połączeniu z poleceniem `contains`. Przykładowy kod wykorzystujący te dwa polecenia może wyglądać następująco:

```
string replace 'tekst do zmodyfikowania' (contains --regex 'wzór') ''
```

W powyższym kodzie podmieniamy pasujące do wzoru znaki na pusty ciąg znaków, co w efekcie usuwa je z tekstu. W poniższych przykładach znajdziesz różne możliwości wykorzystania tej kombinacji poleceń.

### Usuwanie znaków w nazwach plików

Załóżmy, że mamy folder zawierający pliki z rozszerzeniem `.txt` i chcemy usunąć z ich nazw litery `a`. W tym celu możemy skorzystać z poniższego kodu:

```
for file in *.txt
    set new_name (string replace $file (contains --regex 'a') '')
    mv "$file" "$new_name"
end
```

W ten sposób za pomocą pętli `for`, dla każdego pliku z rozszerzeniem `.txt`, zmieniamy jego nazwę, usuwając z niej wszystkie wystąpienia litery `a`.

### Wycinanie znaków z nazw plików

W przypadku gdy chcemy usunąć z nazw plików określony fragment, możemy wykorzystać również wyrażenie regularne w poleceniu `contains`. Przykładowo, jeśli nasze pliki mają nazwy w formacie `numer_[nazwa].txt` oraz `numer_`, a my chcemy usunąć z tych nazw tylko część z numerem oraz nawiasami kwadratowymi, możemy wykorzystać poniższy kod:

```
for file in *.txt
    set new_name (string replace $file (contains --regex '[\d+]_\[(.*)\]') '$1')
    mv "$file" "$new_name"
end
```

W efekcie po wykonaniu tego kodu, nasze pliki będą miały nazwy w formacie `nazwa.txt`.

## Głębszy wgląd

W poradniku skupiliśmy się na prostych przypadkach usunięcia znaków pasujących do wzoru, ale możliwości wykorzystania polecenia `string replace` w połączeniu z `contains` są znacznie szersze. Dzięki wyrażeniom regularnym możemy precyzyjnie kontrolować, które znaki mają zostać usunięte, co daje nam dużą elastyczność w manipulacji tekstem.

## Zobacz też

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [Polecenie `string replace`](https://fishshell.com/docs/current/cmds/string.html#replace)
- [Polecenie `contains`](https://fishshell.com/docs/current/cmds/contains.html)