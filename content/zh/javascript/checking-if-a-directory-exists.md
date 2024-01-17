---
title:                "检查目录是否存在"
html_title:           "Javascript: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

什么是文件目录存在性检测，程序员为什么要这么做？
文件目录存在性检测是指检查某个目录是否真的存在并可以被程序所访问。程序员经常进行这么操作是为了确保它们的程序按预期工作，并避免出现意外错误。

如何实现文件目录存在性检测：
```Javascript 
if (fs.existsSync('/path/to/directory')) {
  console.log('目录存在！')
} else {
  console.log('目录不存在。')
}
```

更深入地了解：
此功能最早于1983年在Unix操作系统中实现，并随后被其他操作系统如Windows所采用。除了使用Node.js内置的fs模块进行目录检测外，还可以使用第三方模块如fs-extra或fs-jetpack来简化代码。

相关资源：
- [fs模块文档](https://nodejs.org/api/fs.html)
- [fs-extra模块文档](https://github.com/jprichardson/node-fs-extra)
- [fs-jetpack模块文档](https://github.com/szwacz/fs-jetpack)