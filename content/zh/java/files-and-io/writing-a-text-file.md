---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:10.548851-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Java\u7684\u65B0I/O\uFF08NIO\uFF09\u5305\
  \uFF08`java.nio.file`\uFF09\u4E3A\u5904\u7406\u6587\u4EF6\u63D0\u4F9B\u4E86\u66F4\
  \u591A\u6837\u5316\u7684\u65B9\u6CD5\u3002\u4EE5\u4E0B\u662F\u4F7F\u7528`Files.write()`\u5199\
  \u5165\u6587\u4EF6\u7684\u7B80\u5316\u65B9\u5F0F\uFF1A."
lastmod: '2024-04-05T21:53:47.964412-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

## 如何操作：


### 使用 `java.nio.file`（标准库）
Java的新I/O（NIO）包（`java.nio.file`）为处理文件提供了更多样化的方法。以下是使用`Files.write()`写入文件的简化方式：

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("第一行", "第二行", "第三行");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("文件成功写入！");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

输出：

```
文件成功写入！
```

### 使用 `java.io`（标准库）
对于更传统的方法，`java.io.FileWriter`是写入文本文件的一个好选择：

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("你好，世界！\n");
            writer.append("这是另一行。");
            System.out.println("文件成功写入！");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

输出：

```
文件成功写入！
```

### 使用 Apache Commons IO
Apache Commons IO库简化了许多操作，包括文件写入。以下是使用`FileUtils.writeStringToFile()`写入文件的方式：

首先，向您的项目添加依赖。如果使用Maven，请包括：

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- 检查最新版本 -->
</dependency>
```

然后，使用以下代码写入文本文件：

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "这是使用Commons IO写入的文本。", "UTF-8");
            System.out.println("文件成功写入！");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

输出：

```
文件成功写入！
```
