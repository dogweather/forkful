---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:53.835945-07:00
description: "Jak to zrobi\u0107: Aby zacz\u0105\u0107 interakcj\u0119 z XML, zwykle\
  \ korzysta si\u0119 z obiektu `MSXML2.DOMDocument`. Interfejs ten umo\u017Cliwia\
  \ \u0142adowanie, parsowanie i\u2026"
lastmod: '2024-03-13T22:44:35.258571-06:00'
model: gpt-4-0125-preview
summary: "Aby zacz\u0105\u0107 interakcj\u0119 z XML, zwykle korzysta si\u0119 z obiektu\
  \ `MSXML2.DOMDocument`."
title: Praca z XML
weight: 40
---

## Jak to zrobić:
Aby zacząć interakcję z XML, zwykle korzysta się z obiektu `MSXML2.DOMDocument`. Interfejs ten umożliwia ładowanie, parsowanie i nawigowanie po dokumentach XML. Poniżej znajduje się prosty przykład demonstrujący, jak załadować plik XML, nawigować po jego strukturze oraz czytać atrybuty i zawartość tekstową.

```basic
' Najpierw upewnij się, że dodałeś odniesienie do "Microsoft XML, v6.0" za pomocą Narzędzia -> Odniesienia
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Ścieżka\Do\Twojego\Pliku.xml") ' Załaduj swój plik XML

' Sprawdź, czy XML został załadowany pomyślnie
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "Błąd ładowania XML:" & xmlDoc.parseError.reason
Else
    ' Nawiguj i czytaj elementy
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath do znalezienia pierwszego <title> w <book>
    MsgBox book.Text ' Pokaż tekst tytułu
End If
```

W powyższym przykładzie kodu tworzymy instancję `MSXML2.DOMDocument60`, ładujemy plik XML, a następnie sprawdzamy, czy wystąpiły błędy. Jeśli nie znajdziemy błędów, nawigujemy do określonego węzła za pomocą XPath i wyświetlamy jego zawartość tekstową.

## Wgłębienie się:
Integracja możliwości obsługi XML w VBA sięga początków lat 2000, kiedy zaczęła rosnąć potrzeba interakcji aplikacji Office z danymi i usługami sieciowymi. Biblioteka `MSXML` czyli Microsoftowe Usługi Rdzenia XML, ewoluowała przez lata, z `MSXML2.DOMDocument60` będącym jedną z najnowszych wersji zalecanych do użytku ze względu na jej ulepszoną wydajność i funkcje bezpieczeństwa.

Chociaż potężne, możliwości obsługi XML w VBA są uważane za mniej wydajne i bardziej niewygodne w porównaniu z nowoczesnymi środowiskami programistycznymi takimi jak XML.etree w Pythonie czy LINQ to XML w C#. Wrodzona rozwlekłość VBA i wymóg ręcznego dodawania oraz zarządzania odniesieniami może odstraszać od szybkiego rozwoju. Co więcej, z nadejściem JSON jako bardziej lekkiego formatu wymiany danych, wielu programistów i aplikacji odchodzi od XML, chyba że wymagana jest interoperacyjność z systemami dziedzicznymi lub specyficznymi usługami przedsiębiorstw.

Jednakże, do zadań wymagających parsowania lub generowania dokumentów XML w kontekście automatyzacji Microsoft Office, wykorzystanie funkcji obsługi XML w VBA pozostaje żywotną, a czasami konieczną metodą. To pozwala na znalezienie równowagi pomiędzy dostępem do bogatego zestawu funkcji aplikacji Office i możliwościami manipulacji strukturyzowanymi danymi, które oferuje XML.
