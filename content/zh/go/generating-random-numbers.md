---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？
生成随机数是一种编程技巧，在这过程中，产生的数字是不确定的。程序员生成随机数来模拟天然随机性，为数据处理或游戏设计提供随机元素。

## 怎么做： 
```Go
package main 

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	randomNumber := rand.Intn(100) //生成0~99的随机数
	fmt.Println(randomNumber)
}
```
上述代码会输出一个0到99以内的随机数，每次运行输出结果都不同。

## 更深入的探讨
在计算机科学早期，生成真正的随机数是一种挑战，因为计算机内部只能遵循指令执行操作。然而，现代编程语言如Go已经为随机数生成提供了便利。

有不同的方式生成随机数，如使用当前时间的时间戳种子或者外部随机源。在上述代码中，我们使用了Unix时间戳（time.Now().UnixNano()）作为种子。这就意味着，除非两次代码执行间隔小于纳秒级别，否则每次生成的随机数都将不同。

## 更多参考：
对于想了解更多关于Go的随机数生成，您可以参阅以下链接：
1. Go官方文档： [math/rand](https://golang.org/pkg/math/rand/)
2. Go入门指南：[随机数](https://tour.golang.org/basics/1)
3. StackOverflow：[How to generate a random number in Go?](https://stackoverflow.com/questions/12321133/how-to-generate-a-random-num-int-in-go)