---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:34:30.692890-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parseowanie daty z ciągu znaków to proces wyłuskiwania informacji o dacie z tekstu. Programiści robią to, by przetworzyć dane wejściowe, np. z formularzy lub logów, do formatów użytecznych w programach i skryptach.

## Jak to zrobić:
```Bash
#!/bin/bash

# Przykład parsowania daty z ciągu znaków
date_string="2023-04-05 14:23:00"
parsed_date=$(date -d "$date_string" '+%Y-%m-%d %H:%M:%S')

echo "Oryginalny ciąg znaków: $date_string"
echo "Sparsowana data: $parsed_date"
```

Output:
```
Oryginalny ciąg znaków: 2023-04-05 14:23:00
Sparsowana data: 2023-04-05 14:23:00
```

## Głębsze spojrzenie:
Parsowanie dat w Bashu polega na wykorzystaniu polecenia `date`, które ma długą historię w systemach Unix i Linux. Działa poprzez konwersję tekstowych reprezentacji daty i czasu do formatu, który Bash może łatwiej przetworzyć. Alternatywą jest użycie zewnętrznych programów jak `awk`, `sed` czy języków skryptowych jak Python, gdzie posługują się one własnymi, bardziej zaawansowanymi metodami parsowania.

Szczegółowo, funkcja `date` w Bash pozwala na formatowanie wyjściowego ciągu znaków z daty (opcja '+%Y-%m-%d %H:%M:%S') i parsowanie niestandardowych formatów daty przy pomocy opcji `-d`. Obraca ona zróżnicowane formaty wejściowe na standardowe daty i godziny.

## Zobacz również:
- Bash manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/index.html
- Date command examples: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
