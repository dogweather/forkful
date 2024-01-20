---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza ostatniej daty z łańcucha polega na przekształceniu daty zapisanej jako łańcuch znaków (np. "2022-02-22") na obiekt Date, który używany jest powszechnie w programowaniu. Zasadniczo, to pozwala nam pracować z datami w bardziej logiczny i wydajny sposób.

## Jak zrobić:

Najprostszym sposobem na przekształcenie stringa do formatu Date w Ruby jest użycie klasy wbudowanej Date i metody `.parse`. Oto przykład:

```Ruby
require 'date'

string_z_data = "2022-02-22"
data = Date.parse(string_z_data)
puts data
```

Gdy uruchomisz to wśród swojego kodu, otrzymasz:

```Ruby
# 2022-02-22
```

## Głębokie wgłębienie

Analiza daty z łańcucha jest praktyką, która ma wiele zastosowań od początku działań programistycznych. Historycznie jest to proces niezbędny do pracy z danymi czasowymi w prosty i skuteczny sposób. 

Alternatywą do `Date.parse` jest `Date.strptime`. Służy do analizy daty z łańcucha, pozwalając określić format łańcucha wejściowego.

```Ruby
require 'date'

string_z_data = "2022-22-02"
data = Date.strptime(string_z_data, "%Y-%d-%m")
puts data
```

W tym przypadku output będzie taki sam jak w poprzednim przykładzie, ale metoda umożliwia bardziej precyzyjne przekształcanie stringów z określonym formatem.

## Zobacz także

Zapoznaj się z następującymi źródłami do głębszego wyjaśnienia:

1. [Oficjalna dokumentacja Ruby - Klasa Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
2. [Poradnik po angielsku o analizie daty z łańcucha](https://www.rubyguides.com/2015/12/ruby-time/) 
3. [Różnice między .parse a .strptime](https://stackoverflow.com/questions/39159373/ruby-date-parse-vs-date-strptime)