---
title:    "Arduino recipe: Writing tests"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Why 

Arduino programming is becoming increasingly popular among hobbyists and beginners due to its ease of use and versatile applications. However, as with any type of coding, it is important to ensure the reliability and functionality of your code. This is where writing tests comes in handy. Writing tests can help you catch errors and bugs before they become a major problem, making your Arduino projects more efficient and successful.

## How To

Writing tests in Arduino can be done using the built-in Arduino Unit Testing library. To begin, first download and install the library in your Arduino IDE. Once installed, you can start writing tests by creating a new tab in your Arduino sketch and naming it "test". Here is an example of a simple test case for a function that adds two numbers:

```Arduino
#include <ArduinoUnit.h>

test(simpleAddition) {
  int result = add(2, 3); // Call the function you want to test
  assertEqual(5, result); // Check if the result is equal to the expected output
}
```

Once you have written your test cases, you can run them by clicking on the "Verify" button in your IDE. This will compile the tests and give you a detailed report of which tests passed and which ones failed. 

## Deep Dive

Writing tests not only helps catch bugs, but it also encourages you to write more modular and reusable code. By breaking down your code into smaller functions, you can easily test and debug each function individually. This also makes it easier to make changes or add new features without affecting the entire code base.

Another benefit of writing tests is that it helps you understand your code better. As you write test cases, you are forced to think about all possible scenarios and inputs for your code, giving you a deeper understanding of how your code works.

Additionally, writing tests can save you time and effort in the long run. Instead of manually testing your code every time you make changes, you can simply run your test cases and ensure that everything is functioning as expected.

## See Also

- [Arduino Unit Testing Documentation](https://github.com/mmurdoch/arduinounit)
- [How to Write Great Arduino Tests](https://blog.brickelectric.com/how-to-write-great-arduino-tests/)
- [Top 5 Arduino Testing Tips for a Rock Solid Project](https://www.tentaclee.com/tutorials/arduino-testing-tips/)