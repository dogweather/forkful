---
title:                "编写文本文件"
date:                  2024-01-19
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
写文本文件就是在电脑上创建并保存文字数据。程序员这么做，是为了记录日志、保存设置或导出数据。

## How to: (如何操作：)
```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteTextFile {
    public static void main(String[] args) {
        String textToWrite = "Hello, this is a sample text.";
        String filePath = "sample.txt";
        
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(filePath))) {
            writer.write(textToWrite);
            System.out.println("File written successfully.");
        } catch (IOException e) {
            System.err.println("Error writing the file: " + e.getMessage());
        }
    }
}
```
运行以上代码后，会创建一个名为 `sample.txt` 的文本文件，内容是 "Hello, this is a sample text." 并输出 "File written successfully."

## Deep Dive (深入了解：)
早期，Java 的 I/O 操作比较复杂，但是随着 `java.nio` 包的出现和更新，文件操作变得更简单。不过，老式 `java.io` 和新式 `java.nio` 都能写文件。另外，可以通过缓冲写入 (BufferedWriter) 提高性能，或者使用 `PrintWriter` 进行格式化输出。

## See Also (另请参阅：)
- [Oracle Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html)
- [Oracle Java NIO File API](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Java Tutorials – Writing I/O](https://docs.oracle.com/javase/tutorial/essential/io/)
