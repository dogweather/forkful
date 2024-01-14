---
title:                "Java: 写文本文件"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

为什么：在日常生活和工作中，我们经常需要记录和存储信息，比如日记、备忘录或者数据记录。编写文本文件是一种简单有效的方法来保存这些信息，以便日后查阅和使用。

如何编写文本文件：

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriter {
	public static void main(String[] args) {
		
		try {
			// 创建一个FileWriter对象，指定要写入的文件名和是否追加写入
			FileWriter writer = new FileWriter("textfile.txt", true);
			
			// 使用write()方法写入字符串
			writer.write("这是一个文本文件。\n");
			writer.write("我正在编写Java博客文章。\n");
			
			// 关闭流
			writer.close();
			
			System.out.println("文本文件已成功写入。");
		} catch (IOException e) {
			System.out.println("写入文件时出现错误。");
			e.printStackTrace();
		}
	}
}
```

输出结果：

文本文件已成功写入。

深入解析：

Java提供了多种类和方法来操作文本文件，如FileWriter和BufferedReader，可以按需选择。建议使用try-catch语句来处理可能出现的IO异常以保证程序的健壮性。同时，可以使用"\n"来实现换行。

请注意，在实际开发中，文本文件的写入操作不仅仅局限于简单的字符串，还可以写入其他对象或者数据，具体操作方法可以查阅相关文档或教程。

##另请参阅：

- 文本文件读取教程: [https://www.javatpoint.com/java-filewriter-class](https://www.javatpoint.com/java-filewriter-class)
- Java IO包文档: [https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html)