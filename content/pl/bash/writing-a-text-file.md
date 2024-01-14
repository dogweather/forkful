---
title:    "Bash: Tworzenie pliku tekstowego"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego pisać plik tekstowy?

Pisanie plików tekstowych jest nieodłączną częścią programowania w Bash. Jest to podstawowa umiejętność, którą każdy programista powinien posiadać, ponieważ pozwala na tworzenie, edycję i wyświetlanie tekstowych danych w systemie operacyjnym. Pliki tekstowe są również jednym z najprostszych sposobów na przechowywanie informacji i dzielenie się nimi z innymi programistami. Dlatego też warto poznać podstawy pisania plików tekstowych w Bash.

## Jak pisać plik tekstowy?

Aby utworzyć nowy plik tekstowy w Bash, wystarczy użyć polecenia "touch" i podać nazwę nowego pliku. Na przykład:

```Bash
touch nowy_plik.txt
```

W ten sposób utworzony zostanie nowy pusty plik o nazwie "nowy_plik.txt". Możemy również użyć polecenia "echo" aby zapisać treść wewnątrz pliku. Na przykład:

```Bash
echo "To jest tekst zapisany w pliku" > nowy_plik.txt
```

W ten sposób plik "nowy_plik.txt" będzie zawierał tekst "To jest tekst zapisany w pliku". Możemy również użyć polecenia "cat" aby wyświetlić zawartość pliku w terminalu. Na przykład:

```Bash
cat nowy_plik.txt
```

W ten sposób zostanie wyświetlona zawartość pliku "nowy_plik.txt" w terminalu.

##Głębszy wgląd w pisanie plików tekstowych

Podstawy pisania plików tekstowych w Bash to tylko wierzchołek góry lodowej. Istnieje wiele różnych poleceń i opcji, które można wykorzystać w celu modyfikacji i zarządzania plikami tekstowymi. Na przykład, można użyć polecenia "cp" aby skopiować istniejący plik tekstowy, lub "rm" aby go usunąć. Można również wykorzystać polecenie "cat" w połączeniu z opcją ">" aby dopisywać tekst do istniejącego pliku tekstowego. Warto także zapoznać się z kolejnymi poziomami trudności, takimi jak praca z plikami CSV lub JSON, lub używanie różnych narzędzi do generowania i analizowania danych tekstowych.

## Zobacz też

- https://www.shellscript.sh/ 
- https://www.tutorialspoint.com/unix/unix-regular-expressions.htm 
- https://www.udemy.com/course/linux-command-line-bash-shell-scripting-dds/?referralCode=7C0E047CB048BDCBDB3C