---
title:    "PHP: Wydobywanie podciągów"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego
Często w programowaniu napotykamy na sytuacje, w których musimy wyciąć część tekstu z ciągu znaków. Może to być potrzebne do analizy danych, zarządzania tekstem lub generowania dynamicznych linków. W tym wpisie przedstawimy wam jak w prosty sposób wyciągać podciągi w języku PHP.

## Jak to zrobić
Wyciąganie podciągów w PHP jest bardzo proste dzięki funkcji `substr()`. Wystarczy podać jako pierwszy argument ciąg znaków, a jako drugi i ewentualnie trzeci argument indeksy początkowy i końcowy podciągu. Przykładowy kod wygląda następująco:
```PHP
$nazwa = "Wpis o wycinaniu podciągów";
echo substr($nazwa, 7); // wyświetli "o wycinaniu podciągów"
```
Jeśli potrzebujemy tylko części tekstu od końca, możemy podać ujemne liczby jako argumenty. Na przykład:
```PHP
$nazwa = "Wpis o wycinaniu podciągów";
echo substr($nazwa, -12); // wyświetli "podciągów"
```
Możemy również wstawić wycięty podciąg w inne miejsce, na przykład:
```PHP
$nazwa = "Wpis o wycinaniu podciągów";
echo "Czytaj więcej na stronie: " . substr($nazwa, 7); // wyświetli "Czytaj więcej na stronie: o wycinaniu podciągów"
```

## Deep Dive
Funkcja `substr()` jest bardzo wszechstronna i pozwala na wycięcie wielu różnych podciągów w zależności od potrzeb. Możemy również wykorzystać trzeci argument, który określa długość wyciętego podciągu. Dodatkowo, funkcja ta działa również na polskich znakach, co jest bardzo przydatne w naszych projektach.

## Zobacz także
- [Dokumentacja PHP: funkcja substr()](https://www.php.net/manual/en/function.substr.php)
- [Wyciąganie podciągów w innych językach programowania](https://www.geeksforgeeks.org/extract-a-substring-from-a-string-using-javascript/)