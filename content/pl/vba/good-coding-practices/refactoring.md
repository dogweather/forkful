---
title:                "Refaktoryzacja"
aliases:
- /pl/vba/refactoring.md
date:                  2024-02-01T22:00:12.995886-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/vba/refactoring.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Refaktoryzacja w programowaniu polega na modyfikowaniu struktury kodu bez zmiany jego zachowania, aby poprawić aspekty takie jak czytelność, łatwość utrzymania czy wydajność. Programiści przeprowadzają refaktoryzację, aby kod był bardziej efektywny, łatwiejszy do zrozumienia, prostszy w modyfikacji w przyszłości i aby zredukować prawdopodobieństwo błędów.

## Jak to zrobić:

Rozważmy podstawowy przykład w Visual Basic for Applications (VBA), gdzie mamy subrutynę, która drukuje szczegóły pracownika. Początkowo kod jest zagracony, trudny do utrzymania lub rozbudowy.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

Refaktoryzacja krok 1: Wyodrębnij metodę. Jedną z najczęstszych technik refaktoryzacji jest przeniesienie określonego fragmentu kodu do własnej metody. Dzięki temu kod staje się bardziej modularny i łatwiejszy do zrozumienia.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    DisplayMessage name, age, department
End Sub

Private Sub DisplayMessage(name As String, age As Integer, department As String)
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

Refaktoryzacja krok 2: Użyj struktury. Ten krok polega na użyciu struktury danych do przechowywania powiązanych danych, co poprawia przejrzystość kodu i ułatwia przekazywanie zagregowanych danych.

```vb
Type Employee
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Employee
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "IT"
    
    DisplayMessage emp
End Sub

Private Sub DisplayMessage(emp As Employee)
    MsgBox "Name: " & emp.name & vbCrLf & "Age: " & emp.age & vbCrLf & "Department: " & emp.department
End Sub
```

Te kroki transformują zagracony kod w modularny, uporządkowany kod, znacznie poprawiając czytelność i łatwość utrzymania.

## Głębsze spojrzenie

Koncepcja refaktoryzacji jest tak stara jak samo programowanie, ale książka Martina Fowlera "Refaktoryzacja: Udoskonalanie struktury istniejącego kodu" wprowadziła ją do głównego nurtu, podkreślając jej znaczenie w procesie rozwoju oprogramowania. W Visual Basic for Applications refaktoryzacja może być nieco bardziej wymagająca ze względu na brak wbudowanych narzędzi, które można znaleźć w nowocześniejszych zintegrowanych środowiskach programistycznych (IDE), wspierających automatyczną refaktoryzację.

Jednakże nie umniejsza to jej znaczenia. Nawet w VBA, stosowanie podstawowych technik refaktoryzacji ręcznie może znacznie ulepszyć bazę kodu, czyniąc ją czystszą i bardziej wydajną. Chociaż VBA nie posiada tych samych nowoczesnych udogodnień, zasady dobrego projektowania kodu pozostają uniwersalne. Programiści, którzy przeszli z innych języków, mogą uznać ręczny proces za uciążliwy, ale niewątpliwie docenią korzyści płynące z inwestycji czasu w poprawę jakości kodu od samego początku.

W przypadku bardziej rozbudowanych środowisk programistycznych lub przy pracy nad szczególnie skomplikowanymi projektami, warto rozważyć alternatywy, które oferują potężniejsze narzędzia do refaktoryzacji lub konwersję projektów VBA na język .NET, gdzie Visual Studio zapewnia obszerną pomoc w refaktoryzacji. Niemniej jednak zrozumienie i stosowanie zasad refaktoryzacji w VBA jest cenną umiejętnością, która podkreśla znaczenie pisania czystego, łatwego w utrzymaniu kodu, niezależnie od środowiska.
