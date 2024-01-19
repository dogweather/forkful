---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
创建临时文件能让我们在不涉及主存储区的情况下编写、读取和存储大量数据，常常用于大文件传输和数据处理。

## 如何操作：
在Node.js中，我们可以使用`tmp-promise`库创建临时文件。请先运行`npm install tmp-promise`，然后看下面的代码示例：

```Javascript
const tmp = require('tmp-promise');

(async () => {
    const { path, cleanup } = await tmp.file();
    console.log("临时文件路径: ", path);

    // 在此添加你的代码
    // ...

    await cleanup(); // 别忘了清理临时文件！
})();
```

## 深度剖析
历史上，生成临时文件在大数据处理和编程语言如Unix shell中是常见的操作。现代语言如JavaScript通常借助第三方库比如`tmp-promise`来实现。

有些情况下，你可能想要直接在内存中处理所有数据，避免写入硬盘。Node.js就提供了`Buffer`和`Stream`来实现。

创建临时文件的内部细节包括文件路径的生成方式以及何时何地应该删除这些文件。大多数库都帮你自动处理这些问题，但务必理解你使用的库具体是怎么工作的。

## 查阅更多
1. [Node.js Buffer](https://nodejs.org/api/buffer.html)
2. [Node.js Stream](https://nodejs.org/api/stream.html) 
3. [tmp-promise library](https://github.com/benjamingr/tmp-promise)