---
title:    "PHP recipe: Writing tests"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why 
Have you ever written a code that seemed perfect, but when you ran it, you encountered unexpected errors? This is where writing tests comes in. By writing tests, you can catch and prevent these errors before they even occur, ensuring a smoother and more efficient programming process.

## How To
Writing tests may seem daunting at first, but with the help of PHP's testing framework, PHPUnit, it becomes much more manageable. Let's take a look at an example of a simple test:

```PHP
// Define a test class
class SimpleTest extends \PHPUnit\Framework\TestCase 
{
    // A simple addition test
    public function testAddition() 
    {
        $result = 5 + 5;
        // The expected result should be 10
        $this->assertEquals(10, $result);
    }
}
```

In the above code, we have defined a simple test class and added a test case to it. The `assertEquals` method compares the expected result (10) to the actual result (5+5), and if they match, the test passes. PHPUnit also provides other useful methods for testing code, such as `assertNotEmpty` and `assertSame`.

Now let's take a look at some sample output when running this test:

```Text
PHPUnit 9.0.1 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 20 ms, Memory: 4.00 MB

OK (1 test, 1 assertion)
```

As you can see, the test was successful, and the output also tells us that it ran 1 test and made 1 assertion. Using these simple examples and methods, you can easily write and run tests for your code.

## Deep Dive
While writing tests, it is essential to keep a few things in mind. First, tests should be independent of each other, meaning one test should not rely on the result of another test. This ensures that tests can be run individually and in any order without affecting the outcome.

Secondly, good tests should have a clear and concise description of what they are testing. This helps in identifying the purpose of each test and makes debugging easier.

Lastly, remember to regularly update and maintain your tests as your codebase evolves. This ensures that your tests continue to check for any errors or changes in your code.

## See Also
For more information and resources on writing tests in PHP, check out these helpful links:

- [PHP Testing with PHPUnit: Running tests](https://phpunit.readthedocs.io/en/9.5/writing-tests-for-phpunit.html)
- [PHPUnit Code Example](https://phpunit.de/getting-started/phpunit-8.html)
- [Tips for Writing Better Tests in PHP](https://dzone.com/articles/tips-for-writing-better-unit-tests-in-php)
- [Testing Tips from PHPUnit's Creator](https://thephp.cc/news/2021/02/tips-from-sebastian-bergmann-creator-of-phpunit)

Now that you have a basic understanding of writing tests in PHP, go ahead and start adding tests to your code. Happy testing!