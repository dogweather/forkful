---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:25.941198-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Haskell\u901A\u8FC7\u5176\u57FA\u7840\
  \u5E93\u63D0\u4F9B\u4E86\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u7684\u76F4\
  \u63A5\u65B9\u6CD5\uFF0C\u4E3B\u8981\u4F7F\u7528`System.Directory`\u6A21\u5757\u3002\
  \u8BA9\u6211\u4EEC\u6765\u770B\u4E00\u4E2A\u57FA\u7840\u793A\u4F8B\uFF1A."
lastmod: '2024-03-13T22:44:47.832342-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u901A\u8FC7\u5176\u57FA\u7840\u5E93\u63D0\u4F9B\u4E86\u68C0\u67E5\
  \u76EE\u5F55\u662F\u5426\u5B58\u5728\u7684\u76F4\u63A5\u65B9\u6CD5\uFF0C\u4E3B\u8981\
  \u4F7F\u7528`System.Directory`\u6A21\u5757\u3002\u8BA9\u6211\u4EEC\u6765\u770B\u4E00\
  \u4E2A\u57FA\u7840\u793A\u4F8B\uFF1A."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 如何操作：
Haskell通过其基础库提供了检查目录是否存在的直接方法，主要使用`System.Directory`模块。让我们来看一个基础示例：

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "该目录是否存在? " ++ show exists
```

示例输出，取决于目录是否存在：

```
该目录是否存在? True
```
或者：
```
该目录是否存在? False
```

对于更复杂的场景或额外的功能，你可能会考虑使用像`filepath`这样的流行第三方库来以更抽象的方式处理和操作文件路径。然而，就简单地检查目录是否存在的目的来说，基础库的`System.Directory`就足够且高效了。

记住，与文件系统工作可能会因平台而异，Haskell的方法旨在抽象化这些差异。始终在目标系统上测试你的文件操作以确保预期行为。
