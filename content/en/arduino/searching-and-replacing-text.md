---
title:    "Arduino recipe: Searching and replacing text"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself typing the same text over and over again in your Arduino code? Or maybe you have a large block of code that needs to be changed in multiple places. This is where the "search and replace" function comes in handy. It allows you to quickly and easily update your code without the hassle of manually changing each instance.

## How To

To use the "search and replace" function in your Arduino code, simply follow these steps:

1. Open your Arduino code in the IDE.
2. Click on the "Edit" tab at the top of the screen.
3. Select "Find and Replace" from the drop-down menu.
4. In the pop-up window, enter the text you want to search for in the "Find" field.
5. Then, enter the replacement text in the "Replace" field.
6. You can choose to either replace all instances at once or go through them one by one.
7. Once you're satisfied with the changes, click the "Replace" or "Replace All" button.

Here's an example of using the "search and replace" function in a simple Arduino sketch:

```Arduino
int ledPin = 13; //setting the pin for the LED
void setup() {
  pinMode(ledPin, OUTPUT); //set the pin as an output
}

void loop() {
  digitalWrite(ledPin, HIGH); //turn the LED on
  delay(500); //wait for half a second
  digitalWrite(ledPin, LOW); //turn the LED off
  delay(500); //wait for half a second
}
```

We want to change the pin number for our LED, which is currently set to 13, to 9. This can easily be done using the "search and replace" function by searching for "ledPin = 13" and replacing it with "ledPin = 9". The revised code would look like this:

```Arduino
int ledPin = 9; //setting the pin for the LED
void setup() {
  pinMode(ledPin, OUTPUT); //set the pin as an output
}

void loop() {
  digitalWrite(ledPin, HIGH); //turn the LED on
  delay(500); //wait for half a second
  digitalWrite(ledPin, LOW); //turn the LED off
  delay(500); //wait for half a second
}
```

## Deep Dive

The "search and replace" function is a powerful tool for making changes in your code. Here are a few things to keep in mind when using it:

- Make sure you use unique words or phrases in your search query to avoid changing unintended code.
- You can use the keyboard shortcut "Ctrl + F" on Windows or "Cmd + F" on Mac to quickly open the "Find and Replace" window.
- Regular expressions can be used in the search field, allowing for more advanced search and replace options.
- The "Replace All" function can be used to quickly make all changes without having to go through each instance manually.

## See Also

- [Arduino IDE Documentation](https://www.arduino.cc/en/guide/)
- [Regular Expressions in Arduino](https://www.arduino.cc/reference/en/language/functions/programming/math/regression-curve-fitting/)