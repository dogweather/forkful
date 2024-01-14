---
title:    "Javascript: 检查目录是否存在。"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 为什么

在编写JavaScript程序时，经常会涉及到检查目录是否存在的情况。这是因为在程序中经常需要读取或操作特定目录中的文件。因此，当我们需要进行文件操作时，首先需要确保所需目录存在，以避免出现错误。

# 怎么做

要检查目录是否存在，我们可以使用`fs.existsSync()`方法。这个方法可以接收一个目录路径作为参数，并返回一个布尔值，指示该目录是否存在。让我们来看一个例子：

```Javascript
let fs = require('fs');
let directoryPath = '/Users/username/Desktop/new_folder';

if (fs.existsSync(directoryPath)) {
  console.log('该目录存在');
} else {
  console.log('该目录不存在');
}
```

在上面的代码中，我们首先导入了Node.js内置的`fs`模块，然后指定了要检查的目录路径。接下来使用`fs.existsSync()`方法来检查目录是否存在，并根据返回值打印不同的结果。

如果目录存在，控制台将输出`该目录存在`，否则将输出`该目录不存在`。

# 深入探讨

除了使用`fs.existsSync()`方法外，我们还可以使用`fs.statSync()`方法来检查目录是否存在。这个方法同样接收一个目录路径作为参数，并返回一个包含目录信息的对象。如果目录不存在，该方法将抛出一个错误。

```Javascript
let fs = require('fs');
let directoryPath = '/Users/username/Desktop/new_folder';

try {
  let stats = fs.statSync(directoryPath);
  // 进行文件操作
  console.log('目录存在');
} catch (err) {
  console.error(err);
}
```

使用这个方法可以更加详细地了解目录的类型和信息。如果目录存在，我们可以继续进行文件操作。如果目录不存在，我们可以通过捕获错误来处理这种情况。

# 参考链接

- [Node.js文件系统模块](https://nodejs.org/api/fs.html)
- [了解Node.js中的文件操作](https://www.runoob.com/nodejs/nodejs-fs.html)
- [使用JavaScript检查目录是否存在](https://blog.csdn.net/weixin_34282386/article/details/85966413)