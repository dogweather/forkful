---
title:    "Go: 向标准错误输出编写"
keywords: ["Go"]
---

{{< edit_this_page >}}

## 为什么

当我们编写Go程序时，我们经常需要调试和检查代码，以确定程序是否正常运行。通过将错误信息打印到标准错误输出，我们可以轻松地识别并解决代码中的错误。

## 如何做

我们可以使用`fmt.Fprintf()`函数将错误信息打印到标准错误输出中，代码示例如下：

```Go
func main() {
	err := someFunction() //调用一个可能出错的函数
	if err != nil {
		fmt.Fprintf(os.Stderr, "发生了错误：%v\n", err)
	}
}
```
运行上述代码，如果`err`不为空，则会将出错信息打印到标准错误输出中。

## 深入探讨

除了使用`fmt.Fprintf()`函数，我们还可以使用`log`包中的函数将错误信息打印到标准错误输出中。例如，可以使用`log.Println()`函数打印一行错误信息，并且会自动添加换行符。

```Go
func main() {
	err := someFunction()
	if err != nil {
		log.Println("发生了错误：", err)
	}
}
```

除了直接将错误信息打印到标准错误输出中，我们也可以使用`log.SetOutput()`函数将错误信息重定向到其他地方，如文件中。

## 参考资料

- [Go标准库文档：fmt.Fprintf](https://golang.org/pkg/fmt/#Fprintf)
- [Go标准库文档：log](https://golang.org/pkg/log/)

## 参见

- [如何使用错误处理来提高Go程序的健壮性](https://example.com/blog/robust-error-handling-go)
- [探索Go标准库中的日志和错误处理](https://example.com/blog/exploring-go-logging-and-error-handling)