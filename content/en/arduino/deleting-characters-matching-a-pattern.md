---
title:    "Arduino recipe: Deleting characters matching a pattern"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a pattern can be a useful tool in Arduino programming. It allows you to remove unwanted characters from input data, making it easier to process and use in your code. This can be particularly helpful when working with data from sensors or serial communication.

## How To

To delete characters matching a pattern in Arduino, we will use the `String` library and its `replace()` function. This function takes in two parameters - the character or pattern you want to replace, and the character you want to replace it with. Let's look at an example:

```
Arduino void setup(){
  Serial.begin(9600);  // initialize serial communication
}

void loop(){
  String data = "Hello, World!"; // create a sample string

  // replace all occurrences of "o" with "e"
  data.replace("o", "e");
  Serial.println(data); // output: Helle, Werld!
}
```

In this example, we first initialize serial communication in the setup function. Then in the loop, we create a `String` variable called `data` with the value "Hello, World!". Using the `replace()` function, we replace all occurrences of "o" with "e" in the string, resulting in "Helle, Werld!". Finally, we print the updated string to the serial monitor.

This method can also be used to remove specific characters from a string. For example, if we want to remove all exclamation marks from a string, we can replace them with an empty string, like this:

```
Arduino void setup(){
  Serial.begin(9600);  // initialize serial communication
}

void loop(){
  String data = "Helle, Werld!"; // our string with an exclamation mark

  // replace all "!" with empty string
  data.replace("!", "");
  Serial.println(data); // output: Helle, Werld
}
```

## Deep Dive

The `replace()` function in Arduino also allows you to replace multiple characters or patterns at once by using arrays. For example, if we want to replace "o" with "e" and "l" with "z" in our string, we can do it like this:

```
Arduino void setup(){
  Serial.begin(9600);  // initialize serial communication
}

void loop(){
  String data = "Helle, Werld!"; // our string with multiple characters to replace

  // create arrays with characters to replace and their replacements
  char toReplace[] = {'o', 'l'};
  char replacement[] = {'e', 'z'};

  // replace characters using arrays
  for(int i=0; i<2; i++){
    data.replace(toReplace[i], replacement[i]);
  }
  Serial.println(data); // output: Heee, Wezrd!
}
```

The `replace()` function can also be used to replace words or phrases with another. Just make sure to include the entire word or phrase and its replacement in double quotes.

## See Also
- [String library documentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [String replace() function reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)