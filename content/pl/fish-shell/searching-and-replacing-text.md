---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Fish Shell: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Czego & Dlaczego?

W programowaniu często musimy zmieniać tekst w naszym kodzie. To znaczy, że zamieniamy jedno słowo lub zdanie na inne. Robimy to, aby nasz kod był łatwiejszy do czytania i zrozumienia dla innych programistów. 

Jak to zrobić:

Fish Shell oferuje prosty i szybki sposób na wyszukiwanie i zamianę tekstu. Aby tego dokonać, wystarczy użyć komendy "sed". W poniższym przykładzie zastępujemy słowo "hello" wyrażeniem "hi" w pliku o nazwie "text.txt".

```Fish Shell
sed -i 's/hello/hi/g' text.txt
```
Output:
```
hi world, hi fish!
```

Możemy także wykonać to samo polecenie na kilku plikach jednocześnie. W tym celu musimy wykorzystać opcję "-exec" oraz komendę "find". Poniższy przykład zamienia "hello" na "hi" w każdym pliku tekstowym znajdującym się w bieżącym katalogu.

```Fish Shell
find . -type f -exec sed -i 's/hello/hi/g' {} \;
```

Deep Dive:

Wyszukiwanie i zmiana tekstu jest jednym z najczęściej wykorzystywanych poleceń w edytorach tekstowych oraz w konkurencyjnych powłokach, takich jak Bash. W Fish Shell wykorzystana jest biblioteka libsed do przetwarzania plików i wyszukiwania wyrażeń regularnych. Można także użyć programu "rs" lub "awk", jeśli wymagane jest bardziej złożone wyszukiwanie tekstu.

Zobacz także:

- Dokumentacja Fish Shell dla polecenia "sed": https://fishshell.com/docs/current/cmds/sed.html
- Przykłady zastosowań "sed": https://www.gnu.org/software/sed/manual/sed.html#Examples