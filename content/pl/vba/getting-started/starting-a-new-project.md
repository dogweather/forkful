---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:13.101268-07:00
description: "Jak to zrobi\u0107: Gdy b\u0119dziesz gotowy, aby rozpocz\u0105\u0107\
  \ nowy projekt VBA, punktem wyj\u015Bcia zazwyczaj jest dost\u0119p do edytora VBA\
  \ i inicjalizacja ram projektu.\u2026"
lastmod: '2024-03-13T22:44:35.232731-06:00'
model: gpt-4-0125-preview
summary: "Gdy b\u0119dziesz gotowy, aby rozpocz\u0105\u0107 nowy projekt VBA, punktem\
  \ wyj\u015Bcia zazwyczaj jest dost\u0119p do edytora VBA i inicjalizacja ram projektu."
title: Rozpoczynanie nowego projektu
weight: 1
---

## Jak to zrobić:
Gdy będziesz gotowy, aby rozpocząć nowy projekt VBA, punktem wyjścia zazwyczaj jest dostęp do edytora VBA i inicjalizacja ram projektu. Przejdźmy przez kroki, korzystając z Excela jako aplikacji hosta:

1. **Otwórz edytor VBA**: W Excelu naciśnij `Alt + F11`, aby uzyskać dostęp do edytora VBA.
2. **Wstaw nowy moduł**: Przejdź do `Wstaw > Moduł` z menu, aby dodać nowy moduł do swojego projektu. Tutaj będzie znajdować się twój kod.
3. **Pisanie pierwszego makra**: Zakodujmy proste makro, które wyświetla okno komunikatu. Wpisz poniższy kod w module:

```vb
Sub SayHello()
    MsgBox "Hello, World!", vbInformation, "Pozdrowienia"
End Sub
```

4. **Uruchom swoje makro**: Naciśnij `F5`, gdy kursor znajduje się wewnątrz subporyncji `SayHello` lub przejdź do `Uruchom > Uruchom Sub/UserForm` i wybierz `SayHello`. Powinno pojawić się okno komunikatu z napisem "Hello, World!" i przyciskiem "OK".

Przykładowy wynik:

```plaintext
Okno komunikatu z napisem "Hello, World!".
```

5. **Zapisz swój projekt**: Przed wyjściem upewnij się, że zapisałeś swoją pracę. Jeśli twój skoroszyt Excela był wcześniej niezapisany, zostaniesz poproszony o zapisanie jako skoroszyt z włączoną obsługą makr (format pliku `.xlsm`).

## Pogłębiona analiza
Visual Basic for Applications jest kamieniem węgielnym w strategiach automatyzacji Microsoftu od momentu jego wprowadzenia w 1993 roku. Pochodzący jako ewolucja swojego poprzednika, MacroBasic, VBA dostarczył bardziej solidne rozwiązanie z lepszą integracją w całym pakiecie Office. Przejście na VBA było kluczowe, oznaczając zmianę w kierunku bardziej złożonych możliwości skryptowania, które wykorzystywały moc pełnoprawnych języków programowania.

Pomimo swojego wieku, VBA pozostaje rozpowszechniony we współczesnych środowiskach biurowych, głównie dzięki jego głębokiej integracji z produktami Office i obszernej bazie kodu starszego typu w wielu organizacjach. Jednak ważne jest, aby zauważyć, że dla nowszych aplikacji opartych na technologii webowej lub dla zadań wymagających większej skalowalności i integracji z aplikacjami spoza pakietu Office, języki i frameworki takie jak Python, ze swoim bogatym ekosystemem bibliotek, czy JavaScript dla skryptów Office oferują bardziej nowoczesne i wszechstronne podejście. Te alternatywy, chociaż wymagają ostrzejszej krzywej uczenia się i konfiguracji, zapewniają szerszą możliwość zastosowania oraz wsparcie dla współczesnych praktyk programistycznych, takich jak kontrola wersji i łańcuchy deweloperskie.
