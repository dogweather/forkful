---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

> ## 是什么，为什么？
删除字符匹配模式是通过识别特定模式或序列，从我们的字符串中删除特定字符的过程。程序员进行此操作以清理数据，使其更易于处理和理解。

> ## 怎么做：
在 Go 中，我们使用 `strings.NewReplacer()` 函数删除匹配模式的字符。看下面的代码示例和输出：

```Go
package main
import (
	"fmt"
	"strings"
)

func main() {
	s := "您好，世界！"
	r := strings.NewReplacer("，", "", "！", "")
	result := r.Replace(s)
	fmt.Println(result)
}
```
上面的程序删除了所有的“，”和“！”字符。输出如下：

```Go
您好世界
```

> ## 深入研究：
删除匹配模式的字符概念源于早期的文本处理和编辑器工具。例如，Unix 的 `sed` 和 `awk` 工具通过识别并删除模式来过滤文本。

==替代方法==
在 Go 中，有`strings.ReplaceAll()`函数，也可删除所有符合某模式的字符。

==实现细节==
`strings.NewReplacer()`函数在内部创建了一个`Replacer`对象。我们调用该对象的`Replace()`方法来执行替换。

> ## 另请参考：
- Go 官方文档关于 `strings.NewReplacer()` 的信息: [ https://golang.org/pkg/strings/#NewReplacer ]
- 关于Go语言中字符串处理的完整指南：[https://yourbasic.org/golang/strings/]
- 删除匹配模式的字符的进一步讨论: [https://stackoverflow.com/questions/48668626/delete-characters-in-a-string-golang]