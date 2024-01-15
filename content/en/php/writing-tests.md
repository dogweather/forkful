---
title:                "Writing tests"
html_title:           "PHP recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Writing tests is an essential part of the development process in PHP and plays a crucial role in ensuring the quality and stability of your code. By writing tests, you can catch bugs and errors before they make it to production, saving you time and effort in the long run.

## How To 
To write tests in PHP, you will need a testing framework. Some popular options include PHPUnit, Codeception, and Behat. In this article, we will focus on PHPUnit.

1. First, install PHPUnit using Composer by running the following command in your project directory: 
```PHP
composer require --dev phpunit/phpunit
```

2. Once PHPUnit is installed, create a new file named "CalculatorTest.php" and add the following code to it:
```PHP
<?php
  use PHPUnit\Framework\TestCase;
  require 'Calculator.php';

  class CalculatorTest extends TestCase {
    public function testAdd(){
      $calculator = new Calculator();
      $this->assertEquals(4, $calculator->add(2,2));
    }
  }
```

3. In the same directory, create another file named "Calculator.php" and add the following code to it:
```PHP
<?php
  class Calculator {
    public function add($a, $b){
      return $a + $b;
    }
  }
```

4. Finally, run PHPUnit by entering the following command in your terminal:
```PHP
./vendor/bin/phpunit CalculatorTest.php
```
You should see an output that says "OK (1 test, 1 assertion)".

## Deep Dive
Let's break down the code in our "CalculatorTest.php" file. We first import the TestCase class from PHPUnit, which contains the necessary methods for writing tests. Then, we require the "Calculator.php" file, which contains the class we want to test.

Next, we create a class named CalculatorTest and extend it from TestCase. Inside our "testAdd" function, we create an instance of the Calculator class and use the "assertEquals" method to compare the expected result (4) with the actual result of calling the "add" method on our calculator object with the parameters 2 and 2.

Writing tests involves creating multiple test cases for each possible scenario and making sure they all pass. This way, we can be confident that our code is functioning correctly.

## See Also
- [PHPUnit - Official Documentation](https://phpunit.de/documentation.html)
- [Codeception - Testing Framework for PHP](https://codeception.com/)
- [Behat - Behavior Driven Development for PHP](https://behat.org/)