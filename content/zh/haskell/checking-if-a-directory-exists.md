---
title:                "检查目录是否存在"
html_title:           "Haskell: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

有时候，我们需要在程序中判断一个文件夹是否存在。这可以帮助我们避免创建重复的文件夹或者执行无用的操作。使用 Haskell 提供的文件夹判断函数，可以让我们更精确地控制程序行为，从而提高代码的效率。

## 怎么做

```Haskell
import System.Directory -- 导入 System.Directory 模块

main = do
    dirExists <- doesDirectoryExist "myFolder" -- 使用 doesDirectoryExist 函数来判断是否存在名为 "myFolder" 的文件夹
    if dirExists
        then putStrLn "文件夹已存在" -- 如果文件夹存在，输出提示信息
        else createDirectory "myFolder" -- 如果文件夹不存在，通过 createDirectory 函数创建该文件夹
```

输出：

```
文件夹已存在
```

## 极致探索

除了上述方法之外，我们还可以使用`catchIOError`函数来捕获可能出现的错误，从而做出更细致的处理。

```Haskell
main = do
    result <- tryIOError (createDirectory "myFolder") -- 使用 tryIOError 函数来捕获可能出现的错误
    case result of
        Left _ -> putStrLn "文件夹已存在或者创建失败"
        Right _ -> putStrLn "文件夹创建成功"
```

输出：

```
文件夹已存在或者创建失败
```

## 参考学习

- Haskell 官方文档：https://www.haskell.org/
- System.Directory 模块文档：https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html