---
title:                "Elm: 编写文本文件"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 为什么
为什么要写一个文本文件？作为一名Elm编程人员，你可能已经知道了答案。但是让我们仍然探索一下这个问题。文本文件可以存储并传输数据，从而使得程序的交互更加方便快捷。举个例子，你可以将一个文本文件作为输入，然后让程序对其进行分析并输出结果。通过使用文本文件，我们可以将数据和程序分离开来，从而使得程序更具可读性和可维护性。

# 如何
首先，我们需要导入Elm的Text模块： 
```Elm 
import Text 

-- 这里可以写下你的代码 
```

接着，我们可以使用Text模块中的`fromString`函数来创建一个文本文件对象。例如，我们可以创建一个包含"Hello World"文本的对象： 
```Elm 
textFile = Text.fromString "Hello World" 
``` 

我们也可以使用`append`函数来在已有的文本文件对象上添加新的文本，例如： 
```Elm 
newTextFile = Text.append textFile "This is a new line" 
``` 
最后，我们可以使用`toString`函数来将文本文件对象转换为字符串，并将其写入到一个文本文件中： 
```Elm 
Text.toString newTextFile -- 将文本文件转换为字符串 
|> File.write "myTextFile.txt" -- 写入到名为"myTextFile.txt"的文本文件中 
``` 

# 深入探究
当我们使用`toString`函数来将文本文件对象转换为字符串时，其实是将文本文件对象转换为JSON格式（JavaScript Object Notation，即JavaScript对象表示法）。这意味着我们也可以使用`toValue`函数将文本文件对象转换为JSON的值。例如，我们可以将一个包含姓名和年龄的文本文件对象转换为JSON对象： 
```Elm 
person = Text.fromString "John, 25" -- 创建文本文件对象 
|> Text.split "," -- 使用逗号将文本文件对象分割为姓名和年龄 
|> List.map Text.trim -- 去除每个元素两侧的空格 
|> List.map Text.toValue -- 将每个元素转换为JSON的值 
|> Object.fromList -- 使用JSON的键-值对来创建对象 

-- 结果为 { "name": "John", "age": 25 } 
``` 

# 查看也许有用的其他链接
- [Elm语言官方网站](https://elm-lang.org/ "Elm语言官方网站") 
- [Elm语言文档](https://elm-lang.org/docs "Elm语言文档") 
- [使用Elm语言写一个简单的文本编辑器](https://www.sitepoint.com/building-a-simple-text-editor-with-elm/ "使用Elm语言写一个简单的文本编辑器")