---
title:                "使用正则表达式"
aliases:
- zh/clojure/using-regular-expressions.md
date:                  2024-02-03T19:16:33.856445-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用正则表达式"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？
正则表达式是处理文本任务（如验证输入、搜索和替换文本）中不可或缺的强大工具，用于模式匹配和数据操作。程序员广泛使用它们来高效、简洁地处理复杂的字符串解析和数据验证任务。

## 如何操作：
Clojure，作为Lisp家族的一份子，提供了一整套丰富的函数，它们与Java的正则表达式能力无缝接口。以下是如何利用它们：

### 基本匹配
要检查字符串是否匹配某个模式，使用`re-matches`。如果成功，它返回整个匹配；否则返回`nil`。

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### 搜索模式
要找到模式的第一次出现，`re-find`是你要去的函数：

```clojure
(re-find #"\d+" "Order 123")  ;=> "123"
```

### 捕获组
使用带有括号的模式和`re-find`来捕获组：

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Phone: 123-4567")]
  (println "Area Code:" area "Code:" code))
;; 输出：Area Code: nil Code: 123
```

### 全局搜索（查找所有匹配项）
Clojure没有像某些语言那样的内置全局搜索。相反，使用`re-seq`来获取所有匹配项的延迟序列：

```clojure
(re-seq #"\d+" "id: 123, qty: 456")  ;=> ("123" "456")
```

### 分割字符串
要基于模式分割字符串，使用`clojure.string/split`：

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### 替换
用`clojure.string/replace`替换匹配某个模式的字符串部分：

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "YYYY")  ;=> "YYYY-04-01"
```

### 第三方库
尽管Clojure的内置支持在大多数情况下已足够，但对于更复杂的情况，考虑使用如`clojure.spec`之类的库，用于健壮的数据验证，以及在Web应用中用正则表达式基于路由和输入验证的反应式DOM操作的`reagent`。

```clojure
;; 使用clojure.spec验证电子邮件的示例
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> true
```

记住，虽然正则表达式强大，但它们也可能使代码难以阅读和维护。审慎使用它们，并尽可能考虑使用更简单的字符串操作函数。
