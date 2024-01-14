---
title:                "Arduino recipe: Getting the current date"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Why

Have you ever wanted your Arduino project to display the current date? Whether you're building a digital clock, a data logger or a smart home system, being able to access the current date can add a useful layer of functionality to your project. In this blog post, we will explore how to get the current date using Arduino and why it can be a valuable skill to have.

##How To

To get the current date using Arduino, we will be using the built-in RTC (Real-Time Clock) module. Before we dive into the code, make sure you have the following materials:

- Arduino board (any model)
- RTC module (such as DS1307 or DS3231)
- Jumper wires

Firstly, connect the SDA and SCL pins of the RTC module to the corresponding pins on the Arduino board. Then, open the Arduino IDE and follow these steps:

1. Install the RTC library by going to **Tools > Manage Libraries** and searching for "RTC".
2. Select the **RTClib** library by Adafruit and click **Install**.
3. Go to **File > Examples > RTClib > RTClib** and open the **rtc_clock** example.
4. Upload the code to your Arduino board.

```Arduino
/* Include the RTClib library */
#include "RTClib.h"

/* Create an instance of the RTCDS1307 class, specifying the pins 
   for SDA and SCL */
RTC_DS1307 rtc(SDA, SCL);

void setup () {
  /* Initialize serial communication to view the output */
  Serial.begin(9600);
  /* Check if the RTC module is detected */
  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  /* If the RTC module is detected, set the date and time */
  rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
}

void loop () {
  /* Create an instance of the DateTime class and retrieve the 
     current date and time from RTC module */
  DateTime now = rtc.now();
  
  /* Print the current date to serial monitor in DD/MM/YYYY format */
  Serial.print(now.day(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.year(), DEC);
  
  delay(1000); // Wait for 1 second
}

```

After uploading the code, open the serial monitor and you should see the current date being displayed in the format DD/MM/YYYY. You can modify the code to display the date in any format you prefer.

##Deep Dive

Now that we have successfully retrieved the current date, let's take a deep dive into how it works. RTC modules have a built-in battery supply which keeps the time and date ticking even when the Arduino board is turned off. This allows us to always have the most updated date and time as long as the module is connected to the Arduino.

In the code, we used the **RTClib** library which has a variety of functions that allow us to access the date and time information from the RTC module. The line `rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));` sets the date and time to the current date and time on your computer. This allows us to have an initial value to work with.

Within the `loop()` function, the `now` object of the `DateTime` class is used to retrieve the current date and time information. We then use the `.day()`, `.month()` and `.year()` functions to get the respective values. These functions return integer values, which we then print to the serial monitor in the specified format.

##See Also

- [Adafruit Guide to DS1307 Real Time Clock](https://learn.adafruit.com/adafruit-ds1307-real-time-clock-breakout-board-kit/overview)
- [Arduino RTC Library Documentation](https://www.arduino.cc/en/Reference/RTC)