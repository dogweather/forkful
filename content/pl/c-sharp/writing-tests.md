---
title:                "C#: Pisanie testów."
simple_title:         "Pisanie testów."
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisanie testów jest ważne

Pisanie testów w C# jest kluczowym elementem procesu programowania, ponieważ pozwala nam na weryfikację poprawności naszego kodu. Poprzez testowanie możemy upewnić się, że nasz program działa zgodnie z oczekiwaniami, a także łatwiej odnaleźć i naprawić ewentualne błędy.

## Jak pisać testy w C#

Aby napisać testy w C#, musimy użyć narzędzia o nazwie NUnit, które jest popularnym frameworkiem do testowania w języku C#. Najpierw należy zainstalować ten framework przy użyciu menedżera pakietów NuGet w naszym projekcie. Następnie, w celu napisania testów, tworzymy nowy plik z kodem, a następnie dodajemy do niego odpowiednie referencje. Poniższy kod to przykładowy test, który sprawdza, czy funkcja dodawania zwraca poprawny wynik:

```C#
[Test]
public void TestSum()
{
   // Given
   int num1 = 2;
   int num2 = 3;
   int expected = 5;

   // When
   int actual = Calculator.Add(num1, num2);

   // Then
   Assert.AreEqual(expected, actual);
}
```

Kod ten korzysta z metody `Assert.AreEqual()` do porównania oczekiwanego wyniku z rzeczywistym wynikiem funkcji dodawania. Jeśli test nie przejdzie pomyślnie, zostanie wyświetlony komunikat o błędzie, co ułatwi nam znalezienie ewentualnych problemów w naszym kodzie.

## Głębszy wgląd w pisanie testów

Pisanie testów jest nie tylko sposobem na sprawdzenie poprawności kodu, ale także na poprawę naszych umiejętności programistycznych. Dzięki testom możemy lepiej zrozumieć działanie naszych funkcji i metod oraz znaleźć potencjalne problemy zanim pojawią się one w produkcji. Dodatkowo, pisanie testów pozwala na łatwiejsze wprowadzanie zmian i dodawanie nowych funkcjonalności do naszego kodu.

Warto również zwrócić uwagę na to, że pisanie testów jest częścią dobrych praktyk programistycznych i jest wymagane w niektórych projektach, a także podczas rekrutacji na stanowisko programisty.

## Zobacz także

- [Dokumentacja NUnit](https://github.com/nunit/docs/wiki)
- [Tutorial o pisaniu testów w C#](https://www.tutorialspoint.com/csharp/csharp_unit_testing.htm)
- [Artykuł o zaletach pisanie testów jednostkowych](https://dailydotnettips.com/why-you-should-start-writing-unit-tests-today/)