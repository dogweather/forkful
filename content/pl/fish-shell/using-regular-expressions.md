---
title:                "Używanie wyrażeń regularnych"
html_title:           "Fish Shell: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wykorzystywanie wyrażeń regularnych to metoda programowania, która pozwala na wyszukiwanie i manipulowanie tekstem w sposób wyrafinowany. Programiści często używają regularnych wyrażeń, ponieważ są potężnym narzędziem do transformacji i analizy danych tekstowych.

## Jak to zrobić:
```Fish Shell``` dostarcza nam funkcję `string match`, która umożliwia użycie wyrażeń regularnych w naszych skryptach. Niech poniższe przykłady będą dla Ciebie jasnym wytłumaczeniem działania wyrażeń regularnych.

#### Wyszukiwanie dopasowań:
```Fish Shell
string match 'kota' 'Lubię koty.'
``` 
```Kota```

#### Zastępowanie tekstu:
```Fish Shell
string match -r -R 'kota' 'Małe kotki.' 'pies'
```
```Małe pieski.```

#### Podział tekstu:
```Fish Shell
string match --split -r '[\.\?\!]' 'Cześć! Jak się masz?'
```
```Cześć```  
```Jak się masz```  

## Głębokie zanurzenie:
Wyrażenia regularne są powszechnie stosowane w programowaniu tekstowym od lat 50-tych. Istnieje wiele alternatywnych narzędzi, takich jak ```grep``` czy ```sed```, jednak składnia ```Fish Shell``` jest prostsza i bardziej przejrzysta. Wewnętrznie, ```Fish Shell``` wykorzystuje język ```C``` oraz bibliotekę ```PCRE``` (Perl Compatible Regular Expressions).

## Zobacz też:
- [Oficjalna dokumentacja wyrażeń regularnych w Fish Shell](https://fishshell.com/docs/current/cmds/string.html#description)
- [Język programowania C](https://www.learn-c.org/)