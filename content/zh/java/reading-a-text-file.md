---
title:                "读取文本文件"
html_title:           "Java: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

什么 & 为什么？
读取文本文件是指在编程中从文件中获取文本内容。程序员经常读取文本文件来处理大量数据或与用户交互。这是一个很酷的功能，因为它可以让程序更富有动态性并与用户产生交互。

如何：
如果您想在Java中读取文本文件，您可以使用以下代码：

```Java
try {
    File file = new File("myfile.txt"); // 替换为您要读取的文件名
    Scanner scanner = new Scanner(file);

    while (scanner.hasNextLine()) {
        String line = scanner.nextLine();
        System.out.println(line);
    }

    scanner.close();
} catch (FileNotFoundException e) { // 捕获异常，防止文件不存在时出错
    e.printStackTrace();
}
```

输出：

```
这是第一行文本
这是第二行文本
这是第三行文本
```
深入探讨：
读取文本文件在计算机科学中是一个重要的功能，它可以追溯到早期的操作系统。在过去，读取文件需要大量的代码和错误处理。但是，现在我们可以使用现代化的编程语言，如Java，来轻松地读取文本文件。

除了使用Java的Scanner类，还有其他方法可以读取文本文件。例如，您可以使用BufferedReader类来提高读取文本文件的性能。此外，您也可以使用第三方库来读取包含特殊格式的文本文件。

相关资源：
- [Oracle官方文档：Scanner类](https://docs.oracle.com/javase/10/docs/api/java/util/Scanner.html)
- [Java教程：使用BufferedReader来读取文本文件](https://www.javatutorialhq.com/java/io/bufferedreader-class-tutorial/)
- [Apache Commons IO文档：处理文本文件](https://commons.apache.org/proper/commons-io/javadocs/api-1.4/org/apache/commons/io/input/package-summary.html)