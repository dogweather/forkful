---
date: 2024-01-26 04:16:47.726459-07:00
description: "Jak to zrobi\u0107: Uruchom PowerShell, a znajdziesz si\u0119 w REPL.\
  \ Wypr\xF3buj cmdlet `Get-Date`."
lastmod: '2024-03-13T22:44:35.631359-06:00'
model: gpt-4-0125-preview
summary: "Uruchom PowerShell, a znajdziesz si\u0119 w REPL."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak to zrobić:
Uruchom PowerShell, a znajdziesz się w REPL. Wypróbuj cmdlet `Get-Date`:

```PowerShell
PS > Get-Date
```

Powinieneś zobaczyć wyjściową datę i godzinę:

```PowerShell
Środa, 31 marca 2023 12:34:56
```

Teraz, łącz polecenia. Posortujmy procesy według zużycia pamięci:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

To wyświetli 5 procesów o największym rozmiarze zestawu roboczego (zużycie pamięci).

## Wgłębienie się
REPL PowerShell ma swoje korzenie w powłoce Unix i innych dynamicznych językach powłok, takich jak Python. Jest to środowisko wykonania poleceń interaktywnych dla pojedynczego użytkownika. W przeciwieństwie do języka kompilowanego, gdzie piszesz całe aplikacje, a następnie je kompilujesz, środowisko REPL pozwala pisać i uruchamiać kod linia po linii. PowerShell obsługuje również wykonywanie skryptów dla większych zadań.

Alternatywy dla Windows obejmują Wiersz polecenia lub inne specyficzne dla języka REPL, takie jak IPython. W świecie Unix/Linux, powłoki takie jak bash czy zsh pełnią podobną funkcję.

Implementacja PowerShell używa aplikacji hostującej do uruchomienia powłoki. Choć PowerShell.exe w Windows jest najbardziej powszechny, inne, takie jak Zintegrowane Środowisko Skryptowe (ISE) czy zintegrowany terminal Visual Studio Code, mogą również służyć jako host.

## Zobacz także
- [O PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
