---
title:                "Praca z yaml"
html_title:           "PowerShell: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Czym jest YAML & Dlaczego Programiści tego Używają?
YAML to format pliku używany do przechowywania danych w czytelnej dla człowieka postaci. Programiści często używają YAML jako alternatywy dla innych formatów, takich jak JSON czy XML, ponieważ jest on łatwiejszy do przyswojenia i pisanie.

## Jak to zrobić:
W PowerShell możemy łatwo pracować z plikami YAML za pomocą modułu `PowerYaml`. Aby go zainstalować, użyj poniższego polecenia:
```PowerShell
Install-Module PowerYaml
```

Aby otworzyć plik YAML w PowerShell, możesz użyć polecenia `Get-Yaml`:
```PowerShell
Get-Yaml -Path C:\Users\user1\example.yaml
```

Możesz także zapisywać zmiany w pliku YAML za pomocą polecenia `Set-Yaml`:
```PowerShell
Set-Yaml -Path C:\Users\user1\example.yaml -Property "key" -Value "new value"
```

Możesz również tworzyć nowe pliki YAML za pomocą polecenia `New-Yaml`:
```PowerShell
New-Yaml -Path C:\Users\user1\new_example.yaml -Property @{"key1" = "value1"; "key2" = "value2"}
```

Możliwe jest także wykorzystanie pętli `Foreach-Object` do iteracji po danych z pliku YAML:
```PowerShell
Get-Yaml -Path C:\Users\user1\example.yaml | Foreach-Object {
    # Przetwórz dane tutaj
    $_.property
}
```

## Głębsza Analiza:
YAML został stworzony w 2001 roku jako alternatywa dla XML-u i szybko stał się popularny wśród programistów ze względu na swoją prostotę. Podobnie jak JSON, jest on formatem tekstowym, ale jego składnia jest bardziej przyjazna dla oka i łatwiejsza do zrozumienia dla ludzi.

Alternatywnie, jeśli nie chcesz używać modułu `PowerYaml`, istnieje możliwość konwertowania plików z YAML do JSON przy użyciu polecenia `ConvertFrom-Json` lub do XML przy użyciu polecenia `ConvertTo-Xml`.

## Zobacz także:
- [Dokumentacja formatu YAML](https://yaml.org/)