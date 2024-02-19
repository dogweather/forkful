---
aliases:
- /pl/vba/working-with-csv/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:23.657463-07:00
description: "Praca z plikami CSV (Comma Separated Values - warto\u015Bci oddzielone\
  \ przecinkami) polega na odczytywaniu z nich danych lub zapisywaniu do nich danych\
  \ w\u2026"
lastmod: 2024-02-18 23:08:49.458354
model: gpt-4-0125-preview
summary: "Praca z plikami CSV (Comma Separated Values - warto\u015Bci oddzielone przecinkami)\
  \ polega na odczytywaniu z nich danych lub zapisywaniu do nich danych w\u2026"
title: Praca z formatem CSV
---

{{< edit_this_page >}}

## Co i Dlaczego?

Praca z plikami CSV (Comma Separated Values - wartości oddzielone przecinkami) polega na odczytywaniu z nich danych lub zapisywaniu do nich danych w formacie plików tekstowych, gdzie pola danych są oddzielone przecinkami. Programiści często wykonują to zadanie, aby ułatwić wymianę danych między różnymi aplikacjami oprogramowania, biorąc pod uwagę prostotę i szerokie przyjęcie formatu CSV w różnych środowiskach programistycznych.

## Jak to zrobić:

Visual Basic for Applications (VBA) ułatwia pracę z plikami CSV za pomocą wbudowanych funkcji i metod, które bezproblemowo pozwalają na odczytywanie z tych plików i zapisywanie do nich. Poniżej znajdują się przykłady ilustrujące podstawowe operacje z plikami CSV.

### Odczytywanie pliku CSV:

```basic
Sub ReadCSV()
    Dim filePath As String
    filePath = "C:\example.csv"
    
    Open filePath For Input As #1
    
    Do Until EOF(1)
        Dim line As String
        Line Input #1, line
        Dim dataFields() As String
        dataFields = Split(line, ",")
        
        'Przetwarzaj tablicę dataFields według potrzeb
        Debug.Print Join(dataFields, ";") 'Przykładowe wyjście pokazujące konwersję z przecinków na średniki
    Loop
    
    Close #1
End Sub
```

### Zapisywanie do pliku CSV:

```basic
Sub WriteCSV()
    Dim filePath As String
    filePath = "C:\output.csv"
    Dim dataToWrite As String
    dataToWrite = "ID,Name,Age" & vbCrLf & "1,John Doe,30" & vbCrLf & "2,Jane Doe,29"
    
    Open filePath For Output As #1
    Print #1, dataToWrite
    Close #1
End Sub
```

Przykładowe wyjście w `output.csv`:
```
ID,Name,Age
1,John Doe,30
2,Jane Doe,29
```

## Szczegółowa analiza

Historycznie, pliki CSV były prostą metodą przechowywania danych tabelarycznych w formacie tekstowym. Prostota ich struktury, gdzie każda linia odpowiada jednemu rekordowi danych, a każde pole w rekordzie jest oddzielone przecinkiem, jest zarówno siłą, jak i ograniczeniem CSV. Format ten nie obsługuje natywnie typów danych, co oznacza, że wszystkie dane są przechowywane jako ciągi znaków, a ciężar konwersji danych na odpowiedni typ spoczywa na programiście.

W Visual Basic for Applications, praca z plikami CSV odbywa się głównie za pomocą podstawowych operacji na plikach, jak pokazano w wcześniejszych przykładach. Nie ma bezpośredniego wsparcia dla parsowania CSV jak w bardziej nowoczesnych językach (np. moduł csv w Pythonie), co zapewnia większą kontrolę i wygodę przy obsłudze danych CSV.

Dla bardziej złożonych operacji lub przy pracy z dużymi plikami CSV programiści mogą znaleźć lepsze alternatywy poza czystym VBA, takie jak wykorzystanie zewnętrznych bibliotek lub użycie innych języków programowania wyposażonych w bardziej zaawansowane możliwości obsługi plików CSV. Jednakże, dla prostych zadań związanych z plikami CSV, proste podejście VBA jest często wystarczające i łatwe do zaimplementowania, oferując szybkie rozwiązanie dla aplikacji opartych na Excelu lub innej automatyzacji oprogramowania Microsoft Office.
