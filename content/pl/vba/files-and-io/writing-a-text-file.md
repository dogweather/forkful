---
aliases:
- /pl/vba/writing-a-text-file/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:13.829313-07:00
description: "Tworzenie pliku tekstowego w Visual Basic for Applications (VBA) polega\
  \ na tworzeniu, modyfikowaniu lub dodawaniu danych tekstowych do plik\xF3w, co jest\u2026"
lastmod: 2024-02-18 23:08:49.454066
model: gpt-4-0125-preview
summary: "Tworzenie pliku tekstowego w Visual Basic for Applications (VBA) polega\
  \ na tworzeniu, modyfikowaniu lub dodawaniu danych tekstowych do plik\xF3w, co jest\u2026"
title: Pisanie pliku tekstowego
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie pliku tekstowego w Visual Basic for Applications (VBA) polega na tworzeniu, modyfikowaniu lub dodawaniu danych tekstowych do plików, co jest podstawowym zadaniem do przechowywania wyników, logowania lub interakcji z innymi aplikacjami. Programiści wykorzystują tę funkcjonalność do automatyzacji raportowania, eksportowania danych lub generowania plików konfiguracyjnych w ekosystemie Microsoft Office.

## Jak to zrobić:

VBA oferuje kilka metod na zapis do pliku, ale jedną z najprostszych dróg jest użycie `FileSystemObject`. Oto krok po kroku, jak stworzyć prosty plik tekstowy i zapisać do niego dane:

1. **Dodaj odniesienie do Microsoft Scripting Runtime**: Najpierw upewnij się, że twój edytor VBA ma dostęp do `FileSystemObject`. Przejdź do Narzędzia > Odniesienia w edytorze VBA i zaznacz "Microsoft Scripting Runtime".

2. **Utwórz Plik Tekstowy**: Poniższy fragment kodu VBA pokazuje, jak utworzyć plik tekstowy i zapisać do niego linię tekstu.

```vb
Sub WriteToFile()
    Dim fso As FileSystemObject
    Set fso = New FileSystemObject
    
    Dim textFile As Object
    ' Parametry CreateTextFile: (NazwaPliku, Nadpisz, Unicode)
    Set textFile = fso.CreateTextFile("C:\yourPath\example.txt", True, False)
    
    ' Napisz linię tekstu
    textFile.WriteLine "Cześć, VBA!"
    
    ' Zamknij plik
    textFile.Close
End Sub
```

Ten skrypt tworzy (lub nadpisuje, jeśli już istnieje) plik o nazwie `example.txt` w określonym katalogu i zapisuje "Cześć, VBA!" do niego, zanim zamknie plik, aby zapisać zmiany.

3. **Przykładowy Wynik**:

Po uruchomieniu powyższego skryptu VBA znajdziesz plik o nazwie `example.txt` z następującą zawartością:

```
Cześć, VBA!
```

## Pogłębienie:

`FileSystemObject` (FSO), część biblioteki Microsoft Scripting Runtime, oferuje bogaty zestaw właściwości i metod do operacji na plikach, idąc dalej niż tradycyjne operacje na plikach oferowane przez VBA (np. `Open`, `Print`#, `Write`#). Oprócz obsługi plików FSO może również manipulować folderami i dyskami, co czyni go potężnym narzędziem do operacji na systemie plików w VBA.

Warto jednak zauważyć, że choć FSO prezentuje bardziej nowoczesne podejście do operacji na plikach w VBA, może wprowadzić dodatkowe obciążenie dla prostych zadań w porównaniu z natywnymi instrukcjami obsługi plików VBA. Ponadto, ponieważ FSO jest częścią zewnętrznej biblioteki, przenośność i kompatybilność z innymi systemami (np. wcześniejszymi wersjami Office, Office na Mac) mogą stanowić problem.

W kontekstach, gdzie wydajność, kompatybilność lub minimalne zależności zewnętrzne są kluczowe, programiści mogą rozważyć użycie wbudowanych w VBA technik obsługi plików. Jednakże, dla bardziej złożonych operacji lub przy pracy w środowisku, gdzie te obawy są złagodzone (takim jak kontrolowane środowisko korporacyjne), korzyści płynące z FileSystemObject często przeważają nad jego wadami.
