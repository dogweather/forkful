---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Arduino recipe: Using regular expressions"
simple_title:         "Using regular expressions"

category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions (regex) let you search text for patterns—think wild card on steroids. Programmers use them to validate input, search strings, and extract data efficiently.

## How to:

Arduino doesn't have built-in regex support, but you can mimic simple pattern checks. For more advanced stuff, consider a regex library like `Regexp`.

```Arduino
#include <Regexp.h>

void setup() {
  Serial.begin(9600);
  
  MatchState ms;
  char result;
  
  ms.Target ("Hello World!");
  result = ms.Match ("(World)");

  if (result > 0) {
    char captured[10]; // Make sure this is large enough to hold your match
    ms.GetCapture (captured, 0);
    Serial.print("Match found: ");
    Serial.println(captured);
  } else {
    Serial.println("No match found.");
  }
}

void loop() {
  // Nothing to do here.
}
```

Sample Output:
```
Match found: World
```

## Deep Dive

Regex came from theoretical computer science and has been around since the 1950s. Perl and other languages have strong regex implementation, but on Arduino, resources are limited, thus no native support. Libraries like `Regexp` are your friend—they take some of the load off, but remember they can be resource-heavy for smaller microcontrollers.

## See Also

Check these for more deets:

- Arduino `Regexp` library: [https://www.arduino.cc/reference/en/libraries/regexp/](https://www.arduino.cc/reference/en/libraries/regexp/)
- `Regexp` library GitHub repo: [https://github.com/nickgammon/Regexp](https://github.com/nickgammon/Regexp)
- Online regex tester (for crafting your regex before implementing): [https://regexr.com/](https://regexr.com/)
