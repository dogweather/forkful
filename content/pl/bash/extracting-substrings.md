---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Operacja wyodrębniania podciągów ("substring") pozwala na wybranie części większego ciągu znaków. Programiści często z niej korzystają, gdy muszą zidentyfikować, zmodyfikować lub użyć określonej części ciągu.

## Jak to zrobić:

Oto przykładowy kod Bash i jego wyjście:

```Bash
str="Witaj w świecie programowania!"
echo ${str:6:5}
```

Wyjście:

```Bash
w świ
```

W tym przykładzie ${str:6:5} wyodrębnia 5 znaków zaczynając od 6 pozycji w ciągu `$str`.

## Głębsze zrozumienie

Bash, dysponując funkcją wyodrębniania podciągów, kontynuuje tradycję powłok UNIX-owskich, które znane są z obsługi operacji na ciągach znaków na wysokim poziomie. Alternatywą dla tej operacji w Bash jest używanie `awk` czy `sed`, ale wyodrębnianie podciągów jest zazwyczaj szybsze i prostrze. Co do szczegółów implementacji, po prostu indeksuje znaki w ciągu znaków i zwraca ciąg składający się z znaków pomiędzy tymi indeksami.

## Zobacz też

Zapoznaj się z tymi linkami, aby uzyskać dodatkowe informacje:

1. [String Operations in Bash](https://www.baeldung.com/linux/string-operations-bash)
2. [Extract Substring in Bash](https://linuxconfig.org/how-to-extract-substring-in-bash)
3. [Bash Substring Documentation](https://tldp.org/LDP/abs/html/string-manipulation.html)