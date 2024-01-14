---
title:    "Arduino recipe: Writing tests"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why Writing Tests for Arduino Programming is Important

When it comes to programming, we often hear the phrase "test early, test often." This advice holds true for Arduino programming as well. Writing tests for your Arduino code can help catch errors before they become bigger problems and ensure that your code runs smoothly. In this blog post, we will discuss the importance of writing tests for Arduino programming and how to do it effectively.

## How To Write Tests for Arduino Programming

Writing tests for Arduino programming involves creating small programs that test the functionality of your Arduino code. These tests can be run on the actual Arduino board or via a simulation software such as Proteus. Let's take a look at a simple example to understand how it works.

```Arduino
#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <Wire.h>
#include <Keypad.h>

// Create instance of LCD and Keypad
LiquidCrystal_I2C lcd(0x27, 16, 2);
const byte ROWS = 4;
const byte COLS = 4;
char keys[ROWS][COLS] = { 
  {'1', '2', '3', 'A'},
  {'4', '5', '6', 'B'},
  {'7', '8', '9', 'C'},
  {'*', '0', '#', 'D'}
};
byte rowPins[ROWS] = { 2, 3, 4, 5 };
byte colPins[COLS] = { 6, 7, 8, 9 };
Keypad keypad = Keypad( makeKeymap(keys), rowPins, colPins, ROWS, COLS );

void setup() {
  lcd.begin();
  lcd.backlight();
  lcd.print("Enter a number: ");
}

void loop() {
  char key = keypad.getKey();
  if (key != NO_KEY) {
    lcd.setCursor(0, 1);
    lcd.print(key);
  }
}
```

In this code, we have created a simple program that displays a message on an LCD and allows the user to enter a number using a keypad. To test this code, we can write a small program that inputs different numbers through the keypad and checks if the correct number is displayed on the LCD. For example:

```Arduino
#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <Wire.h>
#include <Keypad.h>

// Create instance of LCD and Keypad
LiquidCrystal_I2C lcd(0x27, 16, 2);
const byte ROWS = 4;
const byte COLS = 4;
char keys[ROWS][COLS] = { 
  {'1', '2', '3', 'A'},
  {'4', '5', '6', 'B'},
  {'7', '8', '9', 'C'},
  {'*', '0', '#', 'D'}
};
byte rowPins[ROWS] = { 2, 3, 4, 5 };
byte colPins[COLS] = { 6, 7, 8, 9 };
Keypad keypad = Keypad( makeKeymap(keys), rowPins, colPins, ROWS, COLS );

void setup() {
  lcd.begin();
  lcd.backlight();
  lcd.print("Enter a number: ");
}

void loop() {
  char key = keypad.getKey();
  if (key != NO_KEY) {
    lcd.setCursor(0, 1);
    lcd.print(key);
    if (key != '5') { // Change the test number here
      // Test fails if input is not 5
      lcd.setCursor(0, 1);
      lcd.print("Test failed");
    }
  }
}
```

In this example, we have added a check to see if the input is not 5. If it's not, then the test fails and displays a message on the LCD. Similarly, you can write different tests to cover all possible scenarios and ensure the correct functioning of your code.

## Deep Dive into Writing Tests for Arduino Programming

Writing tests for Arduino programming not only helps catch errors, but it also ensures that your code is modular, making it easier to debug and maintain. When writing tests, it's important to follow the best practices, such as keeping the tests in a separate folder and naming them according to the corresponding function or component. Additionally, it's helpful to include comments and descriptive function names to make the code more readable.

Another important aspect of writing tests is to cover both positive and negative scenarios. This means testing the normal functioning of the code as well as testing for any potential errors or exceptions. This ensures that your code is reliable and can handle unexpected inputs or situations.

Lastly, don't forget to regularly run your tests and update them as you make changes to your code. This