---
title:                "Arduino: 处理 csv 数据"
simple_title:         "处理 csv 数据"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

为什么：CSV是一种流行的数据格式，在Arduino编程中用于存储和读取数据。通过使用CSV，您可以轻松地存储和处理任何类型的数据，使您的程序更具灵活性。

如何操作：您可以使用Arduino的`SD`库来读取和写入CSV文件。首先，您需要将SD卡插入Arduino板槽中，然后使用以下代码打开文件：

```Arduino
File file = SD.open("data.csv", FILE_WRITE);
```

接下来，您可以使用`println()`函数写入数据，并使用`close()`函数关闭文件。例如，如果您想要记录传感器读数，您可以这样做：

```Arduino
file.println("Sensor reading 1: " + String(sensor1));
file.println("Sensor reading 2: " + String(sensor2));
file.close();
```

这将在CSV文件中创建两行，每行包含传感器名称和读数。最后，您可以使用以下代码从CSV文件中读取数据：

```Arduino
// 创建变量以存储读取的传感器数据
int sensor1 = 0;
int sensor2 = 0;

// 打开文件
File file = SD.open("data.csv");

// 在文件中读取数据，直到到达文件末尾
while (file.available()) {
  // 使用`readStringUntil()`函数读取每行数据，直到遇到换行符为止
  String line = file.readStringUntil('\n');

  // 使用`substring()`函数将数据分割为传感器名称和读数，并将其转换为数字
  if (line.startsWith("Sensor reading 1:")) {
    sensor1 = line.substring(18).toInt();
  } else if (line.startsWith("Sensor reading 2:")) {
    sensor2 = line.substring(18).toInt();
  }
}

// 关闭文件
file.close();

// 打印读取的数据
Serial.println("Sensor reading 1: " + String(sensor1));
Serial.println("Sensor reading 2: " + String(sensor2));
```

深入了解：CSV文件由逗号分隔的值组成，因此您可以使用`split()`函数来将每行数据分割为值的数组。您还可以使用`write()`和`read()`函数来直接处理CSV文件中的二进制数据。

请参阅：了解更多有关Arduino编程和CSV文件的知识，请参阅以下链接：

- [使用Arduino记录和读取数据的教程](https://arduino.cc/en/Tutorial/ReadWrite)
- [使用SD卡库与Arduino一起使用SD卡](https://arduino.cc/en/Reference/SD)
- [更深入地了解CSV文件的结构和用法](https://en.wikipedia.org/wiki/Comma-separated_values)