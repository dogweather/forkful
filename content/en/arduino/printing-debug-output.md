---
title:    "Arduino recipe: Printing debug output"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why 

Debugging is an essential part of programming. Whether you are a beginner or an experienced developer, understanding the flow of your code and troubleshooting issues is crucial for successful project development. One useful tool for debugging your Arduino projects is printing debug output.

## How To

Printing debug output allows you to track the flow of your code and monitor the values of your variables at specific points in your program. This can help identify potential errors or unexpected behavior in your code.

To print debug output in Arduino, we use the ```Serial.print()``` function. This function allows us to send data from our Arduino board to our computer through the serial connection. To use this function, we need to first initialize the serial communication by adding the line ```Serial.begin(9600)``` in our setup function.

Let's take a look at an example:

```Arduino
int sensorValue = 0; //initializing a variable to store sensor reading

void setup() {
  Serial.begin(9600); // initialize serial communication
}

void loop() {
  sensorValue = analogRead(A0); // read sensor value from analog pin 0
  Serial.print("Sensor value: "); // print the text
  Serial.println(sensorValue); // print the sensor value on a new line
  delay(1000); // wait for 1 second before repeating
}
```

In this code, we are reading the analog values from pin A0 and printing it to the serial monitor. We use the ```Serial.print()``` function to print the text "Sensor value: " and the ```sensorValue``` variable using the ```Serial.println()``` function. This will print the sensor value on a new line each time the code is executed.

To view the debug output, we need to open the serial monitor in the Arduino IDE. You can access the serial monitor by clicking on the magnifying glass icon on the top right corner or by going to ```Tools -> Serial Monitor```.

## Deep Dive

The ```Serial.print()``` function has multiple variations that allow you to print different data types, such as integers, floats, and strings. You can also use it to print multiple values at once and format the output as needed. Check out the [Arduino documentation](https://www.arduino.cc/en/Serial/Print) for more information on these variations.

Additionally, you can use the ```Serial.print()``` function for more than just debugging. It can also be used to send data to other devices or interfaces, such as LCD displays or Bluetooth modules.

## See Also

- [Arduino Serial Communication](https://www.arduino.cc/en/Reference/Serial)
- [Getting Started with Arduino](https://www.arduino.cc/en/Guide/HomePage)