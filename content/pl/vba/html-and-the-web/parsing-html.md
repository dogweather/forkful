---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:02.903772-07:00
description: "Przetwarzanie (parsing) HTML w Visual Basic for Applications (VBA) polega\
  \ na ekstrakcji okre\u015Blonych informacji z dokumentu HTML. Programi\u015Bci robi\u0105\
  \ to, aby\u2026"
lastmod: '2024-03-13T22:44:35.229287-06:00'
model: gpt-4-0125-preview
summary: "Przetwarzanie (parsing) HTML w Visual Basic for Applications (VBA) polega\
  \ na ekstrakcji okre\u015Blonych informacji z dokumentu HTML."
title: "Analiza Sk\u0142adniowa HTML"
weight: 43
---

## Co i dlaczego?

Przetwarzanie (parsing) HTML w Visual Basic for Applications (VBA) polega na ekstrakcji określonych informacji z dokumentu HTML. Programiści robią to, aby zautomatyzować proces odczytywania i obsługi danych z stron internetowych, takich jak scrapowanie treści stron internetowych lub automatyzacja wysyłania formularzy i pobierania danych, w aplikacjach takich jak Microsoft Excel czy Access, które obsługują VBA.

## Jak to zrobić:

W VBA można przetwarzać HTML za pomocą `Microsoft HTML Object Library`. Dodaj odniesienie do tej biblioteki w swoim edytorze VBA, przechodząc do Narzędzia > Referencje i zaznaczając `Microsoft HTML Object Library`. Daje to dostęp do klas do nawigowania i manipulowania dokumentami HTML.

Oto prosty przykład, który pokazuje, jak załadować dokument HTML z pliku i wyodrębnić wszystkie linki (tagi kotwiczne):

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim zawartoscPliku As String
    
    ' Wczytanie zawartości HTML z pliku
    htmlFile = "C:\ścieżka\do\twego\pliku.html"
    Open htmlFile For Input As #1
    zawartoscPliku = Input$(LOF(1), 1)
    Close #1
    
    ' Inicjalizacja dokumentu HTML
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = zawartoscPliku
    
    ' Pobieranie wszystkich tagów kotwicznych
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' Iteracja przez wszystkie elementy kotwiczne i drukowanie atrybutu href
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

Ten skrypt odczytuje zawartość pliku HTML, ładuje ją do obiektu `HTMLDocument`, pobiera wszystkie elementy kotwiczne (`<a>` tagi), a następnie iteruje po nich, drukując atrybut `href` każdego z nich do Okna natychmiastowego.

## W głębi:

Historycznie, przetwarzanie HTML w VBA było nieco kłopotliwe z powodu braku bezpośredniego wsparcia dla nowoczesnych technologii skrobania stron internetowych i obsługi dokumentów. Biblioteka Microsoft HTML Object Library, mimo że potężna, jest nieco przestarzała i może nie radzić sobie równie płynnie z nowoczesnymi standardami internetowymi, jak nowsze technologie.

Dla złożonych zadań związanych z przetwarzaniem HTML i skrobania stron internetowych, zalecane są często alternatywne narzędzia i języki takie jak Python z bibliotekami takimi jak Beautiful Soup czy Scrapy. Te nowoczesne narzędzia oferują większą elastyczność, lepszą wydajność i są bardziej zgodne z aktualnymi standardami internetowymi. Jednak podczas pracy w ekosystemie Microsoft Office, używanie VBA z Microsoft HTML Object Library pozostaje cenną umiejętnością. Umożliwia bezpośrednią manipulację zawartością HTML w sposób, który integruje się bezproblemowo z aplikacjami takimi jak Excel i Access, zapewniając prosty sposób na realizację zadań związanych z podstawową obsługą dokumentów HTML bez konieczności wychodzenia poza znajome środowisko VBA.
