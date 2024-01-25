---
title:                "Logging"
date:                  2024-01-25T02:03:10.870958-07:00
model:                 gpt-4-1106-preview
simple_title:         "Logging"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/logging.md"
---

{{< edit_this_page >}}

## What & Why?
"Logging" is keeping a record of events, transactions, or activities that happen over time in a system. Programmers use it to debug, monitor system health, gather statistics, or even audit usage, making it an indispensable practice for maintaining and understanding the behavior of their code under various conditions.

## How to:
Arduino doesn't come with a built-in logging library like some other environments, but you can implement basic logging to the Serial console with minimal fuss. Here's a quick example to get you started:

```arduino
void setup() {
  // Start the serial communication with the given baud rate
  Serial.begin(9600);

  // Wait for the serial port to connect - only necessary on some boards
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB
  }

  // Log an informational message indicating the setup process is complete
  Serial.println("Setup complete!");
}

void loop() {
  // Simple logger that prints the uptime every second
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Uptime (ms): ");
    Serial.println(currentMillis);

    // Here you could also add error logs, warnings, or other info.
  }
  
  // Rest of your program logic here...
}
```

Sample Serial Output:
```
Setup complete!
Uptime (ms): 1000
Uptime (ms): 2000
Uptime (ms): 3000
...
```

## Deep Dive:
Historically, logging on microcontrollers wasn't as straightforward as on a full-blown operating system. Limited resources meant that every byte counted, and developers needed to be careful not to clog the system. With the advent of more capable boards and the Arduino platform simplifying the process, logging has become more accessible.

While the code above demonstrates logging via the Serial interface, other methods include writing to an SD card, sending data over network to a remote server, or even outputting to a small LCD. 

Implementing a logging system brings about considerations such as rotation, level severity (info, debug, warning, error), and performance impact. On an Arduino, you may need to be mindful of memory constraints when logging complex data structures. For remote logging, security of the transmitted logs is also a concern.

More sophisticated solutions like Syslog, a widely-adopted logging standard, exist outside the Arduino world, but you can integrate third-party libraries that offer similar functionality with various degrees of complexity and resource requirements.

## See Also:
- [Arduino `Serial` reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [SD card logging with Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [SparkFun's Data Logging shield](https://www.sparkfun.com/products/13712)
- [TinyWeb: A practical example of remote logging with Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)