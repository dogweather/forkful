---
title:    "Arduino recipe: Searching and replacing text"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

Searching and replacing text may sound like a mundane task, but it can actually save a lot of time and effort for Arduino programmers. Whether it's fixing errors or making changes to multiple lines of code, using the search and replace function can make the process much quicker and efficient.

## How To

To search and replace text in Arduino, follow these steps:

1. Open your Arduino IDE and open the sketch or code that you want to make changes to.

2. In the menu bar, click on **Edit** and then **Find and Replace** or use the shortcut **Ctrl + H**.

3. In the **Find and Replace** window, enter the text you want to search for in the **Find** field.

4. Enter the replacement text in the **Replace** field.

5. Choose the appropriate options for *Match case*, *Whole word*, and *Regular expression* depending on your requirements.

6. Click on **Find** to locate the first instance of the text, or **Replace** to replace the text immediately.

7. To replace all instances of the text, click on **Replace All**.

Here's an example of how you can use the search and replace function in Arduino:

```Arduino
void setup() {
  pinMode(11, OUTPUT);
}

void loop() {
  analogWrite(11, 128);
  analogWrite(11, 255);
  analogWrite(11, 0);
}
```

Let's say we want to change the pin number from 11 to 9. We can use the search and replace function to do so by following the steps above. The updated code would look like this:

```Arduino
void setup() {
  pinMode(9, OUTPUT);
}

void loop() {
  analogWrite(9, 128);
  analogWrite(9, 255);
  analogWrite(9, 0);
}
```

## Deep Dive

The search and replace function in Arduino is not limited to just finding and replacing text. It also has the option for *Regular expression* which allows for more complex searching and replacing based on patterns.

Regular expressions use special characters to define search patterns. Here are a few examples:

- **.** matches any single character
- **^** matches the start of a line
- **$** matches the end of a line
- **\d** matches any digit
- **\s** matches any whitespace character

Using regular expressions, you can create powerful search and replace patterns to make changes to your code quickly and efficiently.

## See Also

- [Guide to Regular Expressions in Arduino](https://programmingelectronics.com/getting-started-with-regular-expressions-in-the-arduino-ide/)
- [Arduino Search and Replace Tutorial](https://www.youtube.com/watch?v=zJgY2ebDMAg)
- [Arduino Reference for Find and Replace](https://www.arduino.cc/reference/en/language/functions/communication/stream/find-and-replace/)