---
title:                "Writing tests"
date:                  2024-01-19
simple_title:         "Writing tests"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testing checks if your code does what it's supposed to. It saves time by catching bugs early and ensures code changes don't break stuff.

## How to:
We're diving into PHPUnit, a popular PHP testing framework. First, install it with Composer:

```bash
composer require --dev phpunit/phpunit
```

Now, let's write a simple test. Imagine you’ve got a class `Calculator` with an `add` method.

```php
// Calculator.php
class Calculator {
    public function add($a, $b) {
        return $a + $b;
    }
}
```

Here’s how you test it:

```php
// CalculatorTest.php
use PHPUnit\Framework\TestCase;

class CalculatorTest extends TestCase {
    public function testAddition() {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```

Run the test with:

```bash
./vendor/bin/phpunit CalculatorTest
```

Output shows if tests pass or fail.

## Deep Dive
Testing wasn't always a big deal in PHP. Originally, many slapped code together and manually checked if it worked. Now, testing is king. PHPUnit started gaining traction in the 2000s and now it's nearly standard. Alternatives? Sure, there’s PHPSpec and Behat, for starters. Under the hood, PHPUnit uses assertions to compare expected and actual results, and test doubles (mocks, stubs, spies) to mimic external dependencies.

## See Also
- PHPUnit Manual: https://phpunit.de/manual/current/en/index.html
- PHP The Right Way (Testing): http://www.phptherightway.com/#testing
- Mockery (mocking framework for PHPUnit): http://docs.mockery.io/en/latest/
