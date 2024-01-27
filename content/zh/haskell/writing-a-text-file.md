---
title:                "编写文本文件"
date:                  2024-01-19
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
写文本文件就是在电脑上创建含有文字内容的文件。程序员通常这么做来保存数据，比如配置、日志、或用户生成内容。

## 如何做：
```Haskell
-- 引入必要的模块
import System.IO  

-- 主函数，调用 writeFileExample
main :: IO ()
main = writeFileExample

-- 使用writeFile函数写文件
writeFileExample :: IO ()
writeFileExample = do
    let content = "Hello, Haskell!"  -- 文件内容
    writeFile "example.txt" content -- 写入内容到example.txt
    putStrLn "文件已保存！" -- 输出确认信息
```
运行后将在当前目录创建一个包含 "Hello, Haskell!" 文字的 "example.txt" 文件，并在控制台显示 "文件已保存！"。

## 深入了解
写文本文件的能力自编程语言发明以来就存在。`writeFile`是Haskell提供的简洁函数，但底层还有其他方法，比如用`openFile`和`hPutStr`函数打开指定模式的文件并写入。Haskell还允许程序员控制字符编码、换行符处理等；`Data.ByteString`库提供了对二进制文件的支持。

## 另见
- [Haskell官方文档](https://www.haskell.org/documentation/)
- [Learn You a Haskell for Great Good!文件和流](http://learnyouahaskell.com/input-and-output#files-and-streams)
