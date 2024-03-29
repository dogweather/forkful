---
date: 2024-01-20 17:58:51.640634-07:00
description: "Szukanie i zamiana tekstu to podstawy edycji - znajdujesz ci\u0105gi\
  \ znak\xF3w i zamieniasz je na co\u015B innego. Programi\u015Bci robi\u0105 to,\
  \ aby szybko aktualizowa\u0107 kod,\u2026"
lastmod: '2024-03-13T22:44:35.612758-06:00'
model: gpt-4-1106-preview
summary: "Szukanie i zamiana tekstu to podstawy edycji - znajdujesz ci\u0105gi znak\xF3\
  w i zamieniasz je na co\u015B innego. Programi\u015Bci robi\u0105 to, aby szybko\
  \ aktualizowa\u0107 kod,\u2026"
title: Wyszukiwanie i zamiana tekstu
---

{{< edit_this_page >}}

## Co i Dlaczego?
Szukanie i zamiana tekstu to podstawy edycji - znajdujesz ciągi znaków i zamieniasz je na coś innego. Programiści robią to, aby szybko aktualizować kod, poprawiać błędy lub dostosowywać dane.

## Jak to zrobić:
Załóżmy, że chcesz zmienić "hello" na "hi" w tekście. W PowerShell to proste:

```PowerShell
# Szukanie i zamiana tekstu w zmiennej
$text = "hello world"
$newText = $text -replace 'hello', 'hi'
$newText
```

Wyjście będzie wyglądać tak:

```PowerShell
hi world
```

A jeśli zechcesz to zautomatyzować dla wielu plików:

```PowerShell
# Szukanie i zamiana w wielu plikach
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'stary', 'nowy' | Set-Content $_
}
```

Pamiętaj, że `-replace` korzysta z wyrażeń regularnych, więc sprytne triki z nimi również zadziałają.

## Deep Dive
Początki "find and replace" sięgają początków edycji tekstu na komputerach. To nie tylko funkcja notatników i IDE, ale podstawowe narzędzie w skryptach i automatyce. PowerShell udostępnia `-replace` jako część swojej składni, co czyni go bardzo elastycznym.

Alternatywą jest `sed` w Unixach, ale w PowerShell, `Get-Content`, `-replace` i `Set-Content` to potężna kombinacja. Możesz np. użyć `Get-Content` do wczytania pliku, `-replace` do przeszukania ciągów znaków oraz `Set-Content` do zapisania zmian.

Ważne jest, że `-replace` działa z wyrażeń regularnych (regex), co oznacza, że możesz wykonywać bardzo specyficzne zmiany w tekście.

## See Also
- [about_Comparison_Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Comparison_Operators)
- [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Regular_Expressions)
- [PowerShell Gallery](https://www.powershellgallery.com/) - do poszukiwania skryptów społeczności

Pamiętaj, że praktyka czyni mistrza. Korzystając z tych zasobów oraz eksperymentując, szybko staniesz się doświadczonym użytkownikiem PowerShell.
