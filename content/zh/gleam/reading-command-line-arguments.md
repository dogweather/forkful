---
title:    "Gleam: 读取命令行参数"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

什么是命令行参数？为什么要阅读命令行参数？命令行参数是指在运行程序时通过命令行输入的各种设置和指令。通过阅读命令行参数，程序可以根据用户的需求来执行不同的操作，使得程序更加灵活和定制化。

## 为什么

阅读命令行参数可以让程序具有更强的可定制性，允许用户通过命令行输入不同的参数来改变程序的行为。这使得程序可以适应不同的需求和场景，提高程序的使用性和效率。

## 如何

要在Gleam中读取命令行参数，可以使用标准库中的`gleam_io`模块。首先，我们需要导入`gleam_io`模块，然后使用`gleam_io.arguments`函数来获取传入的命令行参数。代码示例如下：

```
use gleam_io

fn main() {
  arguments = gleam_io.arguments()

  case arguments {
    Ok(args) -> gleam_io.println("您输入的参数为：", args)
    Err(err) -> gleam_io.println("出错了：", err)
  }
}
```

输出示例（假设程序命名为`demo.gleam`）：

```
$ gleam run demo.gleam --option1 --option2=value
您输入的参数为：["--option1", "--option2=value"]
```

## 深入了解

在Gleam中，命令行参数以字符串列表的形式存储在`gleam_io.arguments()`函数返回的结果中。如果需要解析参数的具体值，可以使用`gleam_parsec`模块中的函数来实现。例如，如果想要获取`--option2`参数的值，可以使用`gleam_parsec.option()`函数来解析此参数。代码示例如下：

```
use gleam_io
use gleam_parsec

fn main() {
  arguments = gleam_io.arguments()

  case arguments {
    Ok(args) -> {
      case gleam_parsec.option("--option2", args) {
        Ok(value) -> gleam_io.println("您输入的--option2参数的值为：", value)
        Err(err) -> gleam_io.println("出错了：", err)
      }
    }
    Err(err) -> gleam_io.println("出错了：", err)
  }
}
```

输出示例（假设程序命名为`parse_option.gleam`）：

```
$ gleam run parse_option.gleam --option1 --option2=value
您输入的--option2参数的值为：value
```

## 参考链接

- `gleam_io`标准库模块：https://gleam.run/stdlib/gleam_io.html
- `gleam_parsec`标准库模块：https://gleam.run/stdlib/gleam_parsec.html
- Gleam文档：https://gleam.run/