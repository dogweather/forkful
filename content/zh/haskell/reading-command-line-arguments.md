---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
命令行参数用于传递信息给程序。程序员使用它来从用户那里获取程序应如何工作的信息，大大增加了程序的灵活性。

## 怎么做:
在Haskell中，你可以使用System.Environment库来操作命令行参数：

```Haskell
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print args
```
在命令行运行这个程序，如下所示：

```shell
$ ghc main.hs
$ ./main arg1 arg2 arg3
["arg1","arg2","arg3"]
```
这个程序会接收任何您传递的命令行参数（"arg1", "arg2", "arg3"），然后打印它们。

## 深度解析：
(1) 历史背景: Haskell语言是在1987年设计的，这种利用命令行参数的方法是从早期的编程语言如C和FORTRAN中借鉴过来的。
(2) 替代方式: `getProgName` 函数可以获得正在运行的程序的名字，而不是它的参数。
(3) 实现细节: 在Haskell中，命令行参数通过列表的形式展示，列表中每个元素都是一个字符串。

## 另请参见：
* 更深入的操作命令行参数的例子和讨论：[http://book.realworldhaskell.org/read/io.html](http://book.realworldhaskell.org/read/io.html)
* Haskell的`System.Environment`库详细文档：[https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)