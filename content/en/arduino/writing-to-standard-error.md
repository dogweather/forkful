---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error (stderr) reports errors and diagnostics separate from standard output (stdout). It's crucial for debugging and logging, helping developers isolate issues without mixing error messages with regular program output.

## How to:
Arduino doesn’t natively support stderr, but we can mimic it by writing to Serial. Imagine an LED blink program with error-checking:

```Arduino
void setup() {
  Serial.begin(9600);
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  if(!digitalWriteCheck(LED_BUILTIN, HIGH)) {
    Serial.println("Error: Can't set LED high"); // This is our "stderr"
  }
  delay(1000); // Wait for a second
  if(!digitalWriteCheck(LED_BUILTIN, LOW)) {
    Serial.println("Error: Can't set LED low"); // This is our "stderr"
  }
  delay(1000); // Wait for a second
}

bool digitalWriteCheck(int pin, int value) {
  // Pretend this function checks if digitalWrite was successful
  digitalWrite(pin, value);
  // If success return true, let's just always fail for this example
  return false;
}
```

Sample Output:
```
Error: Can't set LED high
Error: Can't set LED low
```

## Deep Dive
Historically, stderr is a standard stream in many operating systems, introduced by Unix. In Arduino, which lacks an operating system, we manually output errors using Serial.print or similar. If you’re logging to a computer, logs can be redirected from Serial to a file, effectively separating them from stdout. Advanced users might use SoftwareSerial to emulate stderr on different hardware serial ports.

## See Also
- Arduino’s official documentation on Serial: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Unix Standard Streams: https://en.wikipedia.org/wiki/Standard_streams
- SoftwareSerial Library: https://www.arduino.cc/en/Reference/SoftwareSerial
