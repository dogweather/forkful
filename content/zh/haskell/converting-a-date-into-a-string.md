---
title:    "Haskell: 将日期转换为字符串"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

当我们进行日期和时间相关的编程时，经常需要将日期转换成字符串来进行操作，例如展示给用户或者存储到数据库中。使用 Haskell 的日期库可以轻松地实现这样的转换，让我们来看看具体的方法吧！

## 如何操作

首先，我们需要安装 `date` 库，可以在终端中使用以下命令来安装：

```Haskell
cabal install date
```

接下来，我们需要导入 `Data.Time` 和 `System.Locale` 模块，代码如下：

```Haskell
import Data.Time
import System.Locale
```

现在，我们可以通过 `formatTime` 函数来将日期转换成字符串，该函数包含三个参数：时间格式、区域设置和日期对象。让我们来看一个示例代码：

```Haskell
-- 创建一个日期对象
let currentDate = getCurrentTime

-- 将日期转换成年-月-日的字符串格式
let dateString = formatTime defaultTimeLocale "%Y-%m-%d" currentDate

-- 输出转换后的字符串
print dateString
```

运行上述代码，输出结果类似于 `2021-11-05`，我们也可以根据需要自定义时间格式来实现更灵活的转换。

## 深入了解

在转换日期成字符串时，我们需要使用 `formatTime` 函数来指定日期格式。常用的格式参数包括：

- `%Y`: 四位数字的年份（例如：2021）
- `%m`: 两位数字的月份（例如：01）
- `%d`: 两位数字的日期（例如：05）
- `%H`: 两位数字的小时（24小时制）
- `%I`: 两位数字的小时（12小时制）
- `%M`: 两位数字的分钟
- `%S`: 两位数字的秒
- `%p`: AM/PM 标识符
- `%Z`: 时区名称

更多的日期格式参数和使用方法可以在 [Haskell 的文档](https://www.haskell.org/hoogle/?hoogle=formatTime) 中找到。

## 参考链接

- [Haskell 日期库文档](https://www.stackage.org/haddock/lts-18.12/time-1.11.3.1/Data-Time.html)
- [Haskell 中的字符串与日期相互转换](https://www.geeksforgeeks.org/haskell-string-datetime-conversion/)
- [Haskell 中的时区和日期操作](https://www.stackage.org/haddock/lts-18.12/time-1.11.3.1/System-Locale.html#v:defaultTimeLocale)