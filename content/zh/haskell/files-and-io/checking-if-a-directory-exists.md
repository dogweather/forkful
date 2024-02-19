---
aliases:
- /zh/haskell/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:25.941198-07:00
description: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\u8BB8\u591A\u7F16\
  \u7A0B\u4EFB\u52A1\u4E2D\u7684\u57FA\u672C\u64CD\u4F5C\uFF0C\u5B83\u5141\u8BB8\u57FA\
  \u4E8E\u76EE\u5F55\u7ED3\u6784\u7684\u5B58\u5728\u6216\u7F3A\u5931\u6765\u8FDB\u884C\
  \u6761\u4EF6\u6027\u52A8\u4F5C\u3002\u8FD9\u5BF9\u4E8E\u6587\u4EF6\u64CD\u4F5C\u3001\
  \u81EA\u52A8\u5316\u811A\u672C\u4EE5\u53CA\u5728\u8F6F\u4EF6\u521D\u6B21\u8BBE\u7F6E\
  \u65F6\u975E\u5E38\u5173\u952E\uFF0C\u4EE5\u786E\u4FDD\u5FC5\u8981\u7684\u76EE\u5F55\
  \u5C31\u4F4D\uFF0C\u6216\u907F\u514D\u76EE\u5F55\u91CD\u590D\u3002"
lastmod: 2024-02-18 23:08:59.190985
model: gpt-4-0125-preview
summary: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\u8BB8\u591A\u7F16\
  \u7A0B\u4EFB\u52A1\u4E2D\u7684\u57FA\u672C\u64CD\u4F5C\uFF0C\u5B83\u5141\u8BB8\u57FA\
  \u4E8E\u76EE\u5F55\u7ED3\u6784\u7684\u5B58\u5728\u6216\u7F3A\u5931\u6765\u8FDB\u884C\
  \u6761\u4EF6\u6027\u52A8\u4F5C\u3002\u8FD9\u5BF9\u4E8E\u6587\u4EF6\u64CD\u4F5C\u3001\
  \u81EA\u52A8\u5316\u811A\u672C\u4EE5\u53CA\u5728\u8F6F\u4EF6\u521D\u6B21\u8BBE\u7F6E\
  \u65F6\u975E\u5E38\u5173\u952E\uFF0C\u4EE5\u786E\u4FDD\u5FC5\u8981\u7684\u76EE\u5F55\
  \u5C31\u4F4D\uFF0C\u6216\u907F\u514D\u76EE\u5F55\u91CD\u590D\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
---

{{< edit_this_page >}}

## 什么与为什么？
检查目录是否存在是许多编程任务中的基本操作，它允许基于目录结构的存在或缺失来进行条件性动作。这对于文件操作、自动化脚本以及在软件初次设置时非常关键，以确保必要的目录就位，或避免目录重复。

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
