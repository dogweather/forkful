---
title:                "文字列の大文字化"
html_title:           "Clojure: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

「何？そしてなんで？」

文字列を大文字にすることは、プログラマーの間でよく行われる操作です。大文字にするとは、文字列の最初の文字を大文字にし、残りを小文字にすることを意味します。プログラマーは、読みやすさや一貫性のために文字列を大文字にすることがあります。

「方法」

```Clojure
(clojure.string/capitalize "hello world")
;; => "Hello world"
(clojure.string/capitalize "Clojure")
;; => "Clojure"
```

「深く掘り下げる」

文字列の大文字化は、ほとんどのプログラミング言語でサポートされています。この機能は、読みやすさやコードの一貫性を確保するために重要です。また、文字列を大文字にすることは、データバリデーションにも役立ちます。文字列の大文字化は、一般的にトップレベルの関数として提供されているため、簡単に使用できます。

「関連情報」

- [Clojureドキュメント](https://clojure.org/guides/learn/strings)
- [Java Stringメソッド](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)