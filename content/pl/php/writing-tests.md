---
title:                "PHP: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-tests.md"
---

{{< edit_this_page >}}

# Dlaczego piszemy testy w PHP?

Testy to nieodłączna część procesu tworzenia oprogramowania. Pomagają nam weryfikować poprawność kodu i uniknąć błędów w produkcie końcowym. W tym artykule dowiesz się, dlaczego warto pisać testy w PHP i jak to zrobić.

## Jak to zrobić?

W celu zaprezentowania procesu pisania testów w PHP, spójrzmy na prosty przykład. Załóżmy, że mamy klasę `Calculator` z metodami `add` i `subtract`. Chcemy napisać testy dla tych metod.

Pierwszym krokiem jest zaimportowanie potrzebnych bibliotek PHPUnit. Następnie możemy zacząć pisać testy, używając składni PHP. Przykład kodu wyglądałby następująco:

```PHP
use PHPUnit\Framework\TestCase;

class CalculatorTest extends TestCase {

    public function testAdd() {
        $calculator = new Calculator();
        $result = $calculator->add(2, 3);

        $this->assertEquals(5, $result);
    }

    public function testSubtract() {
        $calculator = new Calculator();
        $result = $calculator->subtract(6, 2);

        $this->assertEquals(4, $result);
    }
}
```

W powyższym przykładzie importujemy klasę `TestCase` z biblioteki PHPUnit, aby móc dziedziczyć jej funkcjonalności. Następnie tworzymy dwa testy, jeden dla metody `add` i drugi dla `subtract`. W obu przypadkach tworzymy instancję klasy `Calculator` i wywołujemy odpowiednie metody. Na końcu używamy asercji `assertEquals` w celu porównania oczekiwanego wyniku z rzeczywistym.

Po ukończeniu testów możemy uruchomić je za pomocą narzędzia PHPUnit. W przypadku, gdy testy zostaną zakończone niepowodzeniem, otrzymamy dokładne informacje o błędzie, co pozwala nam szybko zlokalizować i naprawić ewentualne problemy w naszym kodzie.

## Deep Dive

Pisanie testów w PHP ma wiele zalet. Po pierwsze, pomaga nam w zwiększeniu jakości naszego kodu. Testy pokrywają różne przypadki użycia, co pozwala nam wykryć błędy wcześniej i łatwiej je naprawić.

Po drugie, testy działają jako dokumentacja naszego kodu. Dzięki nim łatwiej zrozumieć, jakie funkcjonalności oferuje nasza aplikacja i jak z niej korzystać.

I wreszcie, testy umożliwiają nam bezpieczne wprowadzanie zmian w kodzie. Dzięki nim możemy mieć pewność, że nasze zmiany nie spowodują błędów w innych częściach aplikacji.

# Zobacz także

- [Dokumentacja PHPUnit](https://phpunit.de/documentation.html)
- [Artykuł na temat testów w PHP](https://www.toptal.com/qa/how-to-test-php-code)
- [Przykładowy projekt z testami w PHP](https://github.com/sebastianbergmann/phpunit-documentation-samples)