---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
创建临时文件主要是产生一个程序在运行期间会使用到，但完成后即被删除的文件。程序员这样做主要是为了保存中间状态，进行文件操作的测试，以及避免在持久存储中留下不需要的数据。

## 如何操作:
在TypeScript中，我们可以使用`tmp-promise`库来处理临时文件。

```TypeScript
import { file as tmpFile } from 'tmp-promise';

async function createTempFile() {
    const {path, cleanup} = await tmpFile();

    console.log('临时文件路径: ', path);

    // 执行清除
    await cleanup();
}

createTempFile().catch(console.error);
```

这会产生如下输出：

```bash
临时文件路径:  /tmp/tmp-1234abcd
```

执行cleanup后，临时文件会被删除。

## 深度解析:
创建临时文件的过程源远流长，二十世纪80年代的Unix系统中就已存在。尽管有其他方法如使用内存中的数据结构（例：字节数组），但创建临时文件的方法在处理大量数据时既实用又可靠。

`tmp-promise`库就是基于这种方法。它在内部使用`os.tmpdir()`方法生成一个随机的临时文件路径。这个文件路径在各种操作系统中有所不同，但通常位于/tmp或C:\Windows\Temp之中。这种设计模式可以确保临时文件的独一无二性。

## 参考:
- [tmp-promise library](https://www.npmjs.com/package/tmp-promise)
- [os.tmpdir() method](https://nodejs.org/api/os.html#os_os_tmpdir)
- [File handling in Unix](https://en.wikipedia.org/wiki/File_system#Unix_and_Unix-like_systems)
- [Blob object for handling temporary data](https://developer.mozilla.org/en-US/docs/Web/API/Blob)

注意: 创建临时文件并进行操作时，一定要记得执行清除操作，以避免不需要的数据堆积和可能的安全风险。