---
title:                "PHP: Pisanie testów"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego piszemy testy?

Pisanie testów jest nieodłączną częścią procesu tworzenia oprogramowania. Pomaga ono tworzyć bardziej niezawodne i stabilne aplikacje. Testy pozwalają nam również szybciej wykrywać i rozwiązywać błędy, co przyspiesza proces wytwarzania kodu. W tym artykule pokażemy dlaczego warto pisać testy w PHP oraz jak to zrobić.

## Jak napisać testy w PHP

Istnieje wiele różnych narzędzi i frameworków, które pomagają w tworzeniu testów w PHP. Jednym z najpopularniejszych jest PHPUnit. Aby zacząć pisać testy, musimy najpierw zainstalować PHPUnit za pomocą narzędzia Composer. Następnie, w naszym projekcie tworzymy plik z testami, w którym będziemy definiować nasze testy. Możemy to zrobić za pomocą słów kluczowych `class` i `function`, a następnie wykorzystać metody dostarczone przez PHPUnit do asercji, czyli sprawdzania czy dany kod działa poprawnie.

```PHP
class CalculatorTest extends PHPUnit\Framework\TestCase {

  public function testAddition() {
    $calculator = new Calculator();
    $this->assertEquals(4, $calculator->add(2, 2));
  }

  public function testSubtraction() {
    $calculator = new Calculator();
    $this->assertEquals(2, $calculator->subtract(4, 2));
  }

}
```

Na powyższym przykładzie widzimy, jak możemy testować prosty kalkulator, sprawdzając czy metody `add` i `subtract` zwracają poprawne wartości. W celu uruchomienia testów, wystarczy uruchomić narzędzie PHPUnit z poziomu konsoli.

## Głębsza analiza

Pisanie testów nie tylko pomaga nam w szybkim wykrywaniu błędów, ale również zachęca do tworzenia lepszego i bardziej modułowego kodu. Dzięki testom, mamy większą pewność, że zmiany wprowadzone w kodzie nie spowodują nieoczekiwanych problemów. Możemy także testować różne przypadki edge oraz scenariusze, co pozwala nam na lepsze zrozumienie działania naszej aplikacji.

Jednym z najważniejszych zalet pisania testów jest również oszczędność czasu i pieniędzy. Dzięki wykrywaniu błędów już na etapie tworzenia kodu, unikamy sytuacji, w których konieczne jest naprawianie poważnych problemów w już działającym oprogramowaniu.

## Zobacz także

1. PHPUnit - https://phpunit.de/
2. Codeception - https://codeception.com/
3. Behat - https://behat.org/