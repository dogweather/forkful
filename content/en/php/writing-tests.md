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

## What & Why?

Writing tests in programming is the process of creating small pieces of code that verify the functionality of the main code. These tests are important for ensuring that the code is working properly and to catch any errors or bugs before they become bigger issues. Programmers write tests to ensure the quality and stability of their code, improve their code's reliability, and save time in the long run.

## How to:

To start writing tests in PHP, we first need to include the PHPunit testing framework in our code. We can do this by using the `require` function and specifying the path to the PHPunit file. Next, we can create a basic test class with a `testExample` method. Within this method, we can use some assertions provided by PHPunit to verify the output of our code. For example:

```
<?php

require './vendor/phpunit/phpunit/src/Framework/Assert/Functions.php';

class MyTestClass extends PHPUnit_Framework_TestCase
{

    public function testExample()
    {
        $result = 1 + 1;
        $expected = 2;
        $this->assertEquals($expected, $result);
    }

}

```

This code checks if the result of adding 1 and 1 is equal to 2, and if not, it will throw an error. Running this test with PHPunit will give us the following output:

```
PHPUnit 8.4.3 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 72 ms, Memory: 4.00 MB

OK (1 test, 1 assertion)
```

This means our test was successful and our code is functioning as expected.

## Deep Dive:

The concept of writing tests has been around for a long time, with the first automated testing framework for PHP being introduced in 2004. Despite its long history, many developers still overlook the importance of writing tests in their code. However, as software development becomes more complex and time-sensitive, the need for reliable and stable code is greater than ever.

There are different types of testing, such as unit testing, integration testing, and acceptance testing, which serve different purposes. Writing tests also allows for easier refactoring and code maintenance, as tests can quickly identify any issues that arise from changes made to the code. Alternatives to writing tests in PHP include using other testing frameworks like Behat or Codeception, which offer different testing approaches and features.

Implementing tests in PHP can also be done through more complex methods, such as using dependency injection, mocking, and stubbing. These advanced techniques help to isolate and test different parts of the code, making the testing process more efficient and effective.

## See Also:

To learn more about writing tests in PHP, check out the official PHPunit documentation at https://phpunit.de/manual/current/en/index.html. You can also explore other testing frameworks like Behat at https://behat.org/ and Codeception at https://codeception.com/. Keep in mind that the best approach for writing tests may vary depending on the project and its specific requirements.