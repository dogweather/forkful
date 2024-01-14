---
title:    "TypeScript: 检查目录是否存在"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

为什么：为什么要进行检查目录是否存在的操作？检查目录是否存在可以帮助我们避免在程序运行过程中遇到意外的错误，比如尝试从不存在的目录中读取文件。

如何进行检查：为了检查一个目录是否存在，我们可以使用Node.js的fs模块中的statSync()方法。代码如下所示：
```TypeScript
import * as fs from 'fs';
const directoryPath = '/path/to/directory';

try {
  // 使用statSync()方法检查目录是否存在
  const stats = fs.statSync(directoryPath);
  // statSync()方法返回一个fs.Stats对象，我们可以通过isDirectory()方法来判断是否是一个目录
  if(stats.isDirectory()){
    console.log('目录存在');
  } else {
    console.log('该路径不是一个目录');
  }
} catch (err) {
  // 如果目录不存在，statSync()方法会抛出一个错误，我们可以在这里进行处理
  console.error(err);
}
```

深入了解：实际上，检查目录是否存在就是检查我们指定的路径是否存在指向目录的指针。如果目录不存在，就无法创建指向它的指针。这就是为什么我们需要使用statSync()方法来检查目录是否存在。

另外，我们也可以使用异步的fs.stat()方法来进行检查，以避免阻塞主线程。

还有一个需要注意的地方是，检查目录是否存在并不意味着它就一定是一个有效的目录，可能它是一个损坏的目录或者是一个权限受限的目录。

查看也: 如果你想了解更多关于检查目录是否存在的知识，可以查看下面这些链接：

- Node.js fs模块文档：https://nodejs.org/api/fs.html
- Node.js fs.Stats文档：https://nodejs.org/api/fs.html#fs_class_fs_stats
- 如何使用fs模块检查文件和目录是否存在：https://www.digitalocean.com/community/tutorials/nodejs-how-to-use-fs-module
- fs.stat() vs fs.statSync()：https://www.geeksforgeeks.org/node-js-fs-stat-v2-method/