---
title:    "PHP recipe: Writing tests"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests may seem like an extra step in the coding process, but it is crucial for developing high-quality and reliable software. Tests allow you to catch bugs and errors early on, saving time and effort in the long run. They also provide a safety net when making changes to your code, ensuring that everything still functions as intended.

## How To

Writing tests in PHP is a straightforward process. First, you will need to set up a testing framework such as PHPUnit. This framework provides tools for creating and running tests, as well as generating reports and tracking code coverage. Next, you will need to create a separate directory for your tests and write test methods for each function or class in your code.

Let's look at an example using a simple "calculate" function. We want our function to take in two numbers and return their sum.

```
<?php
//Import necessary files
require_once 'vendor/autoload.php';

//Create a test class
class CalculatorTest extends \PHPUnit_Framework_TestCase {

//Method to test calculate function
public function testCalculate() {
//Define input values
$a = 10;
$b = 5;

//Run calculate function and store result in variable
$result = calculate($a, $b);

//Assert that result matches the expected output
$this->assertSame(15, $result);
}
}
```

In this example, we import necessary files, create a test class, and write a method to test our calculate function. We define input values, run the function, and use the `assertSame` assertion to compare the result with the expected output.

Running this test will show us that our function passes the test. However, we can also write tests for edge cases. For example, what happens if one, or both, of the input values is negative? This is where writing tests can help uncover potential errors and improve the overall quality of our code.

## Deep Dive

Writing tests allows you to think more critically about your code and identify potential problems. It also serves as a form of documentation, providing insight into the expected inputs and outputs of your code. Additionally, tests can help with code refactoring and maintenance, as you can easily check if any changes have caused any issues.

It's also worth noting that writing tests does not guarantee perfect code. They can only catch errors that are explicitly tested for. However, they are still a valuable tool in the development process and can greatly improve the reliability and stability of your code.

## See Also

For more information and resources on writing tests in PHP, check out the following links:

- [PHPUnit Documentation](https://phpunit.de/documentation.html)
- [A Beginner's Guide to Writing Tests in PHP](https://www.smashingmagazine.com/2018/02/guide-to-php-testing/)
- [The Importance of Writing Tests for Your Code](https://code.tutsplus.com/tutorials/the-importance-of-writing-tests-for-your-code--net-30077)