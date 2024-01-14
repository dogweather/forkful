---
title:    "Javascript: 读取文本文件"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 为什么要读取文本文件

阅读文本文件是编程的一个重要部分，它可以帮助我们处理大量的文本数据。比如，你可能需要读取一个包含学生名单的文本文件，然后对它进行分析并生成相应的报告。因此，学习如何读取文本文件是非常有用的。

## 如何读取文本文件

读取文本文件的基本步骤如下：

1. 首先，我们需要使用`fs`模块来读取文件。这个模块是Node.js内置的，它提供了一些方法来读取和处理文件。
2. 接下来，我们需要使用`readFileSync`方法来同步读取文件。这意味着程序会等待文件读取完成后再继续执行，而不会跳过读取步骤。
3. 然后，我们可以使用`toString()`方法将读取到的文件内容转换成字符串。
4. 最后，我们可以使用`split()`方法将字符串按照指定的分隔符来分割成数组，并对数组进行进一步处理。

下面是一个简单的例子，展示了如何读取一个包含学生名单的文本文件，并将学生的姓名和成绩打印出来：

```javascript
const fs = require('fs'); // 导入fs模块

const studentList = fs.readFileSync('students.txt'); // 同步读取文件，返回一个Buffer对象
const students = studentList.toString().split('\n'); // 转换成字符串，并按行分割成数组

students.forEach((student) => { // 遍历数组
  const [name, score] = student.split(','); // 再次分割每行，获取学生姓名和成绩
  console.log(`${name}: ${score}`); // 打印出学生姓名和成绩
});
```

输出结果如下：

```
张三: 90
李四: 85
王五: 95
```

## 深入了解文本文件的读取

在本文中，我们只是讨论了如何使用Node.js的`fs`模块来读取文本文件。但实际上，文本文件的读取还涉及到很多其他的知识点，比如编码、流式读取等。如果你想深入了解如何优化文本文件的读取性能，或者如何处理特殊的文本格式，那么你可以继续学习相关的知识。

# 参考链接

- [Node.js文档: fs模块](https://nodejs.org/api/fs.html)
- [Node.js文档: Buffer对象](https://nodejs.org/api/buffer.html)
- [MDN文档: String.prototype.split()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [MDN文档: String.prototype.toString()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toString)
- [阮一峰的网络日志: 深入浅出Node.js（五）：编码和字符串](http://www.ruanyifeng.com/blog/2011/06/node_js_encodings.html) 

# 参见

- [Node.js入门教程](https://wangdoc.com/nodejs/fs.html)
- [如何使用Node.js解析文本文件](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)
- [Node.js文档](https://nodejs.org/zh-cn/docs/)