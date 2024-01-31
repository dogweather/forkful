---
title:                "检查目录是否存在"
date:                  2024-01-19
html_title:           "Bash: 检查目录是否存在"
simple_title:         "检查目录是否存在"

category:             "Arduino"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (什么及为什么？)
检查目录是否存在是确认特定路径下的文件夹是否已被创建的过程。程序员这么做是为了避免错误如试图访问不存在的文件夹，或者在不必要的情况下重复创建目录。

## How to: (怎么做：)
```arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // 等待串行端口连接。仅需于使用 Leonardo、Micro 或者 Boarduino 等微控制器时。
  }

  if (!SD.begin(4)) {
    Serial.println("初始化SD卡失败");
    return;
  }

  if (SD.exists("/example")) {
    Serial.println("/example 目录存在");
  } else {
    Serial.println("/example 目录不存在");
  }
}

void loop() {
  // 这里不需要代码
}
```

示例输出：
```
/example 目录存在
```
或
```
/example 目录不存在
```

## Deep Dive (深入解析)
在Arduino的早期版本中，主要通过SD库与SD卡交互。`SD.exists()` 函数就是用来检查文件或目录是否存在。这对于防止文件覆盖、避免写入错误和合理管理存储空间是很重要的。

传统上，当你创建一个新文件前，你需要检查它所在的目录是否存在。如果不存在，你可以使用 `SD.mkdir()` 创建一个新目录。随着时间发展，除了SD库之外，还有其他选择，比如SdFat库，提供了更高的效率和更多的功能。

在实施过程中，你可能会遇到不同类型的存储介质和文件系统，这是检查目录存在性的一大挑战，例如，FAT16/32文件系统与新兴的exFAT或NTFS等不同。

## See Also (另请参阅)
- 官方SD库文档：[Arduino - SD](https://www.arduino.cc/en/reference/SD)
- SdFat库GitHub页面：[SdFat](https://github.com/greiman/SdFat)
- Arduino存储技术讨论：[Arduino Forum Storage](https://forum.arduino.cc/index.php?board=12.0)
