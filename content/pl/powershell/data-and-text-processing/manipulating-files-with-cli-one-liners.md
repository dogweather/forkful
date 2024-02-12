---
title:                "Manipulowanie plikami za pomocą jednolinijkowców CLI"
aliases:
- pl/powershell/manipulating-files-with-cli-one-liners.md
date:                  2024-01-27T16:21:32.865679-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manipulowanie plikami za pomocą jednolinijkowców CLI"

tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/manipulating-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Manipulowanie plikami przy użyciu jednolinijkowców CLI w PowerShell to sprawa szybkiej zmiany, przenoszenia lub uzyskiwania danych pliku bezpośrednio z linii poleceń. Programiści robią to dla efektywności; jest to szybsze niż przeglądanie GUI czy pisanie długich skryptów dla prostych zadań.

## Jak:

### Czytanie pliku
Aby szybko wyświetlić zawartość pliku, użyj polecenia `Get-Content`:
```PowerShell
Get-Content .\example.txt
```

### Zapisywanie do pliku
Aby coś nowego zapisać w pliku, można użyć `Set-Content`:
```PowerShell
Set-Content -Path .\example.txt -Value "Cześć, PowerShell!"
```

### Dodawanie do pliku
Dodawanie danych na końcu pliku bez wymazywania jego zawartości można wykonać za pomocą `Add-Content`:
```PowerShell
Add-Content -Path .\example.txt -Value "Dodaję tę linię."
```

### Kopiowanie plików
Kopiowanie pliku jest proste z `Copy-Item`:
```PowerShell
Copy-Item -Path .\example.txt -Destination .\kopia_example.txt
```

### Usuwanie plików
Aby usunąć plik, po prostu użyj `Remove-Item`:
```PowerShell
Remove-Item -Path .\niechciany_plik.txt
```

### Szukanie w plikach
Użyj `Select-String` do szukania tekstu w plikach:
```PowerShell
Select-String -Path .\*.txt -Pattern "PowerShell"
```

### Łączenie poleceń
PowerShell naprawdę błyszczy swoją zdolnością do łączenia poleceń za pomocą rur. Oto jak można znaleźć pliki i skopiować je do nowego katalogu:
```PowerShell
Get-ChildItem -Path .\*.log | Copy-Item -Destination C:\Logs
```

## Wgłębienie

Historycznie, PowerShell został wprowadzony jako bardziej potężna alternatywa dla tradycyjnego wiersza poleceń w Windows, oferując bezprecedensowy dostęp do wnętrza systemu i jego składowych danych. Łączy szybkość linii poleceń z elastycznością skryptowania, czyniąc go nieocenionym narzędziem dla administratorów systemów i programistów opierających się na Windows.

Alternatywy dla PowerShell do manipulowania plikami obejmują narzędzia oparte na Unixie, takie jak `sed`, `awk`, `grep` i skrypty `bash` dla użytkowników Linuxa i MacOS. Chociaż te narzędzia są niezwykle potężne i mają swoje zalety, PowerShell oferuje głęboką integrację ze środowiskami Windows.

Godnym uwagi aspektem PowerShell jest jego obiektowa natura. W przeciwieństwie do wielu języków skryptowych, które traktują wszystko jako łańcuchy znaków lub strumienie bajtów, PowerShell pracuje bezpośrednio z obiektami .NET. Oznacza to, że kiedy manipulujesz plikami, pracujesz z bogatymi obiektami, które oferują mnóstwo właściwości i metod, ułatwiając zarządzanie złożonymi zadaniami.

Jedną ze słabości PowerShell, szczególnie dla użytkowników Linuxa i MacOS, jest jego postrzegana rozwlekłość w porównaniu do skryptów bashowych czy używania narzędzi linii poleceń Unix. Ponadto, głęboka integracja PowerShell z Windows czasami może utrudniać tworzenie skryptów wieloplatformowych, chociaż wysiłki związane z PowerShell Core mają na celu efektywne zniwelowanie tej luki.

Nie zważając na swoje słabości, moc PowerShell leży w jego potężnych jednolinijkowcach, zintegrowanym środowisku skryptowym i wszechstronnym dostępie do ekosystemu Windows, czyniąc go niezbędnym narzędziem dla tych, którzy chcą manipulować plikami i znacznie więcej bezpośrednio z linii poleceń.
