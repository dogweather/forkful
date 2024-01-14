---
title:    "Clojure: 检查目录是否存在"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么要检查是否存在目录？

检查一个目录是否存在是在编程过程中很常见的一个操作。当我们想要在特定的位置创建文件或者读取某个目录下的文件时，我们需要先确认该目录是否存在。这是一种防范措施，避免程序出错。

# 如何检查一个目录是否存在？

```Clojure
(defn check-dir-exists [dir]
  "Checks if the given directory exists"
  (if (ex