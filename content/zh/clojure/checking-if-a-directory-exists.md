---
title:    "Clojure: 检查目录是否存在"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# 为什么

在进行编程时，经常会遇到需要检查某个文件夹是否存在的情况。这样可以确保程序能够顺利运行，避免出现错误。

# 如何做到

```Clojure
(require '[clojure.java.io :as io])

(defn dir-exists? [path]
  (.exists (io/file path)))

(println (dir-exists? "/Users/username/Documents"))
```

运行以上代码，将会输出 `true` 或者 `false`，取决于您指定的文件夹是否存在。

# 深入了解

通过使用 `java.io.File` 类中的 `.exists` 方法，我们可以检查指定路径是否存在。但是，请注意，这种方法只能检查路径是否存在，无法判断文件夹是否为空或者可写，因此在使用时还需结合其他方法来完成更详细的判断。

# 参考资料

## [clojure.java.io API文档](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io.file)

## [Java中的文件操作](https://www.w3cschool.cn/java/java-files.html)