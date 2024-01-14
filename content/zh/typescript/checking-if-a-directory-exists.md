---
title:    "TypeScript: 检查目录是否存在。"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

为什么：为了保证程序的运行顺利，我们经常会需要检查某个目录是否存在。在进行文件操作、配置路径或者加载资源时，检查目录是否存在可以避免出现错误。

如何做到：在TypeScript中，我们可以使用内置的Node.js函数`fs.existsSync()`来判断目录是否存在。

```TypeScript
// 导入fs模块
import * as fs from 'fs';

// 检查目录是否存在
if(fs.existsSync('./myFolder')){
    console.log('目录存在');
}else{
    console.log('目录不存在');
}
```

输出：

```
目录存在
```

深入了解：`fs.existsSync()`函数的原理是通过调用Node.js的`fs.statSync()`函数来检查目录是否存在。它会返回一个`fs.Stats`对象，其中包含了目录的详细信息。如果目录不存在，则会抛出一个错误。

除了`fs.existsSync()`，我们也可以使用`fs.accessSync()`函数来检查目录是否存在。它与`fs.existsSync()`的不同之处在于，它会尝试访问目录来检查其权限，因此更适合用来判断是否有权限对目录进行操作。

```TypeScript
// 导入fs模块
import * as fs from 'fs';

// 检查目录是否存在并具有可读权限
fs.accessSync('./myFolder', fs.constants.R_OK, (err) => {
    if (err) {
        console.log('目录不存在或不可读');
    }else{
        console.log('目录存在且可读');
    }
});
```

输出：

```
目录存在且可读
```

另外，我们还可以使用`fs.mkdirSync()`函数来创建目录，如果目录已存在则会抛出一个错误。因此，在创建目录前，我们可以先使用`fs.existsSync()`或`fs.accessSync()`来判断目录是否存在，避免重复创建目录。

总结：检查目录是否存在可以帮助我们避免不必要的错误，并且在操作文件时可以提高程序的健壮性。在选择使用哪种方法时，可以根据具体需求来决定。

参考链接：
- [Node.js官方文档 - fs模块](https://nodejs.org/api/fs.html)
- [Node.js官方文档 - fs.stat](https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback)
- [CNode - fs.existsSync()的原理](https://cnodejs.org/topic/5ae8e05e038c554f6755c4f0)
- [CNode - fs.access()用法详解](https://cnodejs.org/topic/5693fa9439efda5c4088ed72)

# 查看也可

- [Node.js官方文档 - fs模块](https://nodejs.org/api/fs.html)
- [TypeScript官方文档 - Node.js类型声明](https://www.typescriptlang.org/docs/handbook/declaration-files/by-example.html#type-checking-the-module)