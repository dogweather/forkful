---
title:    "Python: 获取当前日期"
keywords: ["Python"]
---

{{< edit_this_page >}}

为什么：获取当前日期是编程中必不可少的功能。无论是在应用程序和网页上，都需要准确地显示当前日期来提供更好的用户体验。

如何做：在Python中，获取当前日期非常简单。只需要使用内置的datetime模块即可。以下是一个示例代码：

```Python
import datetime

# 获取当前日期
today = datetime.date.today()
# 打印输出当前日期
print(today)
```

运行以上代码，你会得到类似于以下的输出：

```
2021-05-22
```

深入了解：使用datetime模块可以获取更多关于当前日期的信息。例如，可以通过调用today对象的year、month和day属性来获取当前日期的年、月和日。也可以使用strftime方法来自定义日期的显示格式。详细的使用方法可以参考Python官方文档中的datetime模块说明。

另外，还可以通过安装第三方库如pytz来处理不同时区的日期。这些都可以帮助你更灵活地使用日期功能。

同时，也要注意时间和日期的格式在不同国家和地区可能有所差异，因此在编程中要根据实际情况进行处理。

见下文：如有需要，可以参考下方链接获取更多关于Python中日期处理的相关信息。

### 查看也可以 (See Also):
- [Python官方文档 - datetime模块](https://docs.python.org/3/library/datetime.html)
- [pytz官方文档](https://pypi.org/project/pytz/)
- [Python日期处理教程](https://realpython.com/python-dates/)