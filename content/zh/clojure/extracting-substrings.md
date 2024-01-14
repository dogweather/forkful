---
title:    "Clojure: 提取子字符串"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么要提取子字符串？

提取子字符串是在编程中常见的操作，它可以帮助我们从一个大的字符串中获取我们想要的信息。通过提取子字符串，我们可以更轻松地处理和操作文本数据，使得编程更加高效。

# 如何提取子字符串？

要提取子字符串，我们可以使用Clojure中提供的`substr`函数。它的语法如下：

```Clojure
(substring s start end?)
```

其中，`s`是要提取子字符串的原始字符串，`start`是要开始提取的索引位置，`end`是可选的参数，表示要结束提取的索引位置，默认值为字符串的长度。下面是一个例子：

```Clojure
(def sentence "我爱编程") 
(substring sentence 1 3) 
```

这段代码的输出将会是`爱编`，因为我们从索引位置1开始，一直提取到索引位置3之前的字符。如果没有指定`end`参数，那么默认会提取到字符串的最后一个字符。

# 深入挖掘

除了提取固定位置的子字符串外，我们还可以通过Clojure中提供的`re-find`函数来提取符合特定条件的子字符串。它可以使用正则表达式来搜索并提取字符串中符合条件的子字符串。

下面是一个例子，假设我们有一个字符串列表，其中包含了不同国家的电话号码，我们希望提取出所有的中国手机号码：

```Clojure
(def phone-list ["+8613912345678" "+447812345678" "+8613576543210"])
(def regex #"\+86\d{11}")

(filter #(re-find regex %) phone-list)
```

上述代码中，我们定义了一个正则表达式来匹配中国手机号码的格式，并将它保存在`regex`变量中。然后我们使用`filter`函数来遍历字符串列表，并使用`re-find`函数来判断每个字符串是否符合正则表达式的条件。最终，我们可以得到一个只包含中国手机号码的列表。

# 查看也许感兴趣的内容

- [Clojure官方网站（中文）](https://clojure.org/guides/getting_started)
- [正则表达式在Clojure中的应用](https://clojuredocs.org/clojure.string/re-find)
- [提取子字符串的更多方法和技巧](https://www.baeldung.com/string-substring-performances)