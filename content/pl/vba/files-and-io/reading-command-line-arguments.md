---
title:                "Czytanie argumentów z linii poleceń"
aliases:
- /pl/vba/reading-command-line-arguments.md
date:                  2024-02-01T22:01:25.082124-07:00
model:                 gpt-4-0125-preview
simple_title:         "Czytanie argumentów z linii poleceń"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/vba/reading-command-line-arguments.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?

Odczytywanie argumentów linii komend w Visual Basic for Applications (VBA) polega na dostępie do parametrów przekazanych do programu podczas jego wykonania. Technika ta jest często stosowana, aby wpłynąć na zachowanie lub wynik programu bez potrzeby interakcji użytkownika, co znacznie upraszcza zadania automatyzacji i skryptowania, czyniąc je bardziej wszechstronnymi.

## Jak to zrobić:

W przeciwieństwie do bardziej prostych środowisk programistycznych, VBA nie posiada wbudowanej funkcji bezpośredniego odczytu argumentów linii komend w tradycyjnym sensie, ponieważ jest przede wszystkim przeznaczony do wbudowania w aplikacje Microsoft Office. Jednak, dzięki odrobinie kreatywności, możemy użyć Windows Script Host (WSH) lub wywołać zewnętrzne API, aby osiągnąć podobną funkcjonalność. Oto praktyczne obejście przy użyciu WSH:

1. **Utwórz skrypt VBScript, aby przekazać argumenty do VBA:**

   Na początku, napisz plik VBScript (*yourScript.vbs*), który uruchomi Twoją aplikację VBA (np. makro Excela) i przekaże argumenty linii komend:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Dostęp do Argumentów w VBA:**

   W Twojej aplikacji VBA (*YourMacroWorkbook.xlsm*), zmodyfikuj lub stwórz makro (*YourMacroName*), aby przyjmowało parametry:

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "Argument 1: " & arg1 & " Argument 2: " & arg2
End Sub
```

3. **Uruchom swój skrypt:**

   Wykonaj skrypt VBScript z linii komend, przekazując argumenty według potrzeb:

```shell
cscript yourScript.vbs "Hello" "World"
```

   To powinno skutkować wykonaniem Twojego makra VBA z argumentami "Hello" i "World", które zostaną wyświetlone w oknie wiadomości.

## W głąb tematu:

W kontekście historycznym, VBA został zaprojektowany, aby rozszerzyć możliwości aplikacji Microsoft Office, a nie jako samodzielne środowisko programistyczne. Dlatego bezpośrednia interakcja z linią komend wykracza poza jego główny zakres, co tłumaczy brak wbudowanego wsparcia dla odczytu argumentów linii komend.

Przedstawiona powyżej metoda, choć skuteczna, jest raczej obejściem niż natywnym rozwiązaniem, wykorzystując skrypty zewnętrzne, aby zasypać lukę. Podejście to może wprowadzać złożoność i potencjalne zagrożenia bezpieczeństwa, ponieważ wymaga włączenia makr i potencjalnie obniżenia ustawień bezpieczeństwa, aby je wykonywać.

Do zadań silnie opierających się na argumentach linii komend lub wymagających bardziej płynnej integracji z systemem operacyjnym Windows, inne języki programowania, takie jak PowerShell lub Python, mogą oferować bardziej solidne i bezpieczne rozwiązania. Te alternatywy zapewniają bezpośrednie wsparcie dla argumentów linii komend i są lepiej przystosowane do samodzielnych aplikacji lub skryptów, które wymagają zewnętrznego wprowadzenia, aby dynamicznie modyfikować ich zachowanie.
