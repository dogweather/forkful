---
title:                "Arduino recipe: Searching and replacing text"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

As an Arduino programmer, you may come across the need to search and replace text within your code. This can be useful for making changes to multiple lines of code at once or fixing typos quickly. In this blog post, we will explore the process of searching and replacing text in Arduino programming.

## How To

To search and replace text in your Arduino code, follow these simple steps:

1. Open your Arduino IDE and open the sketch or code where you want to make changes.
2. Click on "Edit" in the toolbar and then select "Find and Replace" from the drop-down menu.
3. In the search bar, type the text you want to replace and in the "Replace with" bar, type the new text you want to use.
4. Click on "Replace All" to replace all instances of the old text with the new one.

Here is an example of searching and replacing the word "Hello" with "Hi" in an Arduino sketch:

```Arduino
#include <LiquidCrystal.h>

LiquidCrystal lcd(12, 11, 5, 4, 3, 2);

void setup() {
  lcd.begin(16, 2);
  //Searching and replacing text
  lcd.print("Hello, world!");
}

void loop() {
  //nothing to see here
}
```

After using the "Find and Replace" tool, the output on the LCD screen would be:

```Arduino
#include <LiquidCrystal.h>

LiquidCrystal lcd(12, 11, 5, 4, 3, 2);

void setup() {
  lcd.begin(16, 2);
  //Searching and replacing text
  lcd.print("Hi, world!");
}

void loop() {
  //nothing to see here
}
```

## Deep Dive

While searching and replacing text may seem simple, there are a few things to keep in mind. First, the "Find and Replace" tool is case sensitive, so be sure to double-check your spelling and capitalization. Second, using the "Replace All" button will make changes throughout the entire file, so use this feature with caution.

In addition, the "Find and Replace" tool also allows you to use regular expressions for more advanced search and replace functions. Regular expressions are a powerful tool for finding text patterns and replacing them with new text. You can learn more about regular expressions and how to use them in Arduino programming from the links in the "See Also" section below.

## See Also

- [Arduino Find and Replace Feature](https://www.arduino.cc/en/Tutorial/FindReplace)
- [Regular Expressions in Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/serial-available/)