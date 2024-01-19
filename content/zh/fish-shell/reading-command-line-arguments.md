---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么和为什么?
命令行参数是指在运行程序命令后面附加的参数。程序员使用它们来修改程序的运行方式或传递数据。

## 如何使用：
在Fish Shell中，你可以通过```argv```变量访问命令行参数。下面是一个简单的示例：
```
Fish Shell
#test_script.fish
echo $argv[1]
```
运行这个脚本输出第一个传递的命令行参数：
```
./test_script.fish Hello
Hello
```
## 深入挖掘
命令行参数的使用可以追溯到Unix的早期日子。其他Shell Script也提供了类似的实现，例如Bash的```$1```，```$2```对应第一个和第二个参数。

Fish Shell提供了更丰富的数组表示法，你可以用```set -l```设定局部变量来优化你的代码。如下面的代码：
```
Fish Shell
set -l first $argv[1]
echo $first
```
上述代码设定了一个局部变量first，被用来存储和输出第一个命令行参数。

## 查看更多
1. Fish Shell官方文档： https://fishshell.com/docs/current/index.html
2. 命令行参数教程：https://ryanstutorials.net/bash-scripting-tutorial/bash-input.php