---
title:                "Usuwanie znaków pasujących do wzoru"
html_title:           "C#: Usuwanie znaków pasujących do wzoru"
simple_title:         "Usuwanie znaków pasujących do wzoru"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego 

Często zdarza się, że w trakcie programowania musimy usuwać znaki, które pasują do pewnego wzorca. To może być konieczne na przykład podczas procesowania tekstu lub danych wejściowych. W tym artykule dowiesz się, dlaczego i w jaki sposób można usuwać znaki pasujące do wzorca w języku C#.

## Jak to zrobić

Do usuwania znaków pasujących do wzorca w C# można użyć metody `Regex.Replace ()`. Przykładowy kod wykorzystujący tę metodę wygląda następująco:

```C#
string input = "To jest przykładowy tekst123, który zawiera liczby.";
string pattern = @"\d"; // wzorzec pasujący do cyfr

string output = Regex.Replace(input, pattern, ""); // usuwanie znaków pasujących do wzorca
Console.WriteLine(output); // wyświetlenie wyniku: "To jest przykładowy tekst, który zawiera liczby."
```

W powyższym przykładzie użyty został wzorzec `\d`, który oznacza cyfry od 0 do 9. Dzięki temu za pomocą metody `Regex.Replace ()` usunięte zostały wszystkie cyfry z tekstu wejściowego. Warto zauważyć, że jako trzeci argument podajemy pusty ciąg znaków, co oznacza, że znaki pasujące do wzorca zostaną zastąpione przez nic.

Można również użyć metody `Regex.Replace ()` w połączeniu z wyrażeniami regularnymi, które umożliwiają jeszcze większą precyzję w określaniu wzorca. Na przykład, jeśli chcemy usunąć wszystkie znaki specjalne z tekstu, można użyć wyrażenia regularnego `[^\w\s]`, które oznacza wszystkie znaki poza literami, cyframi i spacjami. Następnie, w trzecim argumencie metody `Regex.Replace ()`, podajemy pusty ciąg znaków, co spowoduje usunięcie wszystkich znaków specjalnych z tekstu.

## Zagłębianie się w temat

Metoda `Regex.Replace()` jest bardzo przydatna i potrafi rozwiązać wiele problemów związanych z usuwaniem znaków pasujących do wzorca. Warto jednak pamiętać, że proces ten może nie być zawsze efektywny, szczególnie w przypadku dużych tekstów. W takim przypadku lepszym rozwiązaniem może być użycie strumieni (ang. streams) do odczytywania i przetwarzania danych w trakcie usuwania znaków pasujących do wzorca.

## Zobacz też
- [Dokumentacja Microsoft o metodzie Regex.Replace ()](https://docs.microsoft.com/pl-pl/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
- [Poradnik wyrażeń regularnych w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expressions)