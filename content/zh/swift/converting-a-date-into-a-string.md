---
title:    "Swift: 把日期转换成字符串"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 为什么

日期是编程中一个常见的概念，但是在某些情况下，我们需要将日期转换为字符串格式。这可能是为了显示给用户，或者作为数据传输的一部分。在本篇文章中，我们将讨论如何使用Swift将日期转换为字符串，并解释为什么这一步骤很重要。

## 如何

在Swift中，我们可以通过使用`DateFormatter`类来将日期转换为字符串。下面是一个示例代码，演示了如何将当前日期转换为字符串格式：

```Swift
let currentDate = Date() // 当前日期
let dateFormatter = DateFormatter() // 创建日期格式化器
dateFormatter.dateFormat = "MM/dd/yyyy" // 设置日期格式
let stringDate = dateFormatter.string(from: currentDate) // 将日期转换为字符串
print(stringDate) // 打印输出：05/05/2021
```

在上面的代码中，我们创建了一个`DateFormatter`实例，并通过设置`dateFormat`属性来定义日期的格式。然后，我们使用`string(from:)`方法将日期转换为字符串，并将其存储在一个新的字符串变量中。最后，我们使用`print()`函数将字符串日期打印输出。

除了上面的示例，我们也可以通过使用不同的日期格式来转换日期。例如，下面是一个将日期转换为全中文日期格式的示例：

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy年MM月dd日" // 设置日期格式为年月日
let stringDate = dateFormatter.string(from: currentDate)
print(stringDate) // 打印输出：2021年05月05日
```

## 深入探讨

在上面的示例中，我们使用了`DateFormatter`类的`dateFormat`属性来定义日期格式。这个属性实际上是一个字符串，我们可以使用不同的格式符来定义日期的显示方式。这些格式符包括：

- `yyyy`：4位数表示的年份
- `MM`：2位数表示的月份
- `dd`：2位数表示的日
- `HH`：2位数表示的小时数（24小时制）
- `hh`：2位数表示的小时数（12小时制）
- `mm`：2位数表示的分钟数
- `ss`：2位数表示的秒数
- `aaa`：上午/下午的标识符

除了单个格式符外，我们还可以使用符号`/`、`-`、`.`、`:`等来分隔日期中的各个部分。例如，`yyyy-MM-dd HH:mm:ss`表示的是年-月-日 时:分:秒的形式。

总的来说，转换日期为字符串的过程并不难，但是我们需要注意定义日期的格式，以便正确显示日期和时间。在实际开发中，我们还可以进一步学习`DateFormatter`类提供的其他功能，例如转换为不同时区的日期时间等。

## 查看更多

- [Apple官方文档：DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift中日期处理的详细指南](https://nshipster.com/nsformatter/)
- [使用DateFormatter对日期进行格式化](https://www.hackingwithswift.com/example-code/system/how-to-convert-a-date-to-a-string-using-dateformatter)