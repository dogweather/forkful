---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:58.107741-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728Java\u4E2D\uFF0C\u6709\u51E0\u79CD\u65B9\
  \u6CD5\u53EF\u4EE5\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\uFF0C\u4E3B\u8981\
  \u4F7F\u7528`java.nio.file.Files`\u548C`java.io.File`\u7C7B\u3002 **\u4F7F\u7528\
  `java.nio.file.Files`**: \u8FD9\u662F\u6700\u8FD1\u7248\u672C\u7684Java\u63A8\u8350\
  \u7684\u65B9\u6CD5\u3002"
lastmod: '2024-04-05T22:38:46.801588-06:00'
model: gpt-4-0125-preview
summary: "\u5728Java\u4E2D\uFF0C\u6709\u51E0\u79CD\u65B9\u6CD5\u53EF\u4EE5\u68C0\u67E5\
  \u76EE\u5F55\u662F\u5426\u5B58\u5728\uFF0C\u4E3B\u8981\u4F7F\u7528`java.nio.file.Files`\u548C\
  `java.io.File`\u7C7B\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 如何操作:
在Java中，有几种方法可以检查目录是否存在，主要使用`java.nio.file.Files`和`java.io.File`类。

**使用`java.nio.file.Files`**:

这是最近版本的Java推荐的方法。

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // 在这里指定目录路径
        String directoryPath = "path/to/directory";

        // 检查目录是否存在
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("目录存在。");
        } else {
            System.out.println("目录不存在。");
        }
    }
}
```
**示例输出**:
```
目录存在。
```
或 
```
目录不存在。
```

**使用`java.io.File`**:

虽然推荐使用`java.nio.file.Files`，但是较旧的`java.io.File`类也可以使用。

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // 在这里指定目录路径
        String directoryPath = "path/to/directory";

        // 创建一个File对象
        File directory = new File(directoryPath);

        // 检查目录是否存在
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("目录存在。");
        } else {
            System.out.println("目录不存在。");
        }
    }
}
```
**示例输出**:
```
目录存在。
```
或
```
目录不存在。
```

**使用第三方库**:

虽然标准的Java库通常已足够完成此任务，但像Apache Commons IO这样的第三方库提供了可能在更复杂应用中有用的额外文件处理实用程序。

**Apache Commons IO**:

首先，将Apache Commons IO依赖项添加到您的项目中。然后，您可以使用它的特性来检查目录的存在。

```java
// 假设已将Apache Commons IO添加到项目中

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // 在这里指定目录路径
        String directoryPath = "path/to/directory";

        // 使用FileUtils来检查
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("目录存在。");
        } else {
            System.out.println("目录不存在。");
        }
    }
}
```

**注意**：`FileUtils.directoryContains`方法用于检查目录是否包含特定文件，但通过将`null`作为第二个参数传递，您可以用它来检查目录的存在。需要注意的是，这可能不是此方法最直接或预期的使用方式。

**示例输出**:
```
目录存在。
```
或
```
目录不存在。
```
