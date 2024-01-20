---
title:                "テキストファイルの書き方"
html_title:           "Clojure: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 何をしているの？
テキストファイルを書くとは、プログラマーがコードやデータなどをテキスト形式で保存することを指します。このようにすることで、ファイルを読みやすく、修正しやすくすることができます。

また、テキストファイルを使用することで、他のプログラマーやシステムとのやり取りを簡単に行うことができます。これにより、コードやデータを共有することができ、コラボレーションを効率的に行うことができます。

## どのようにするの？
```Clojure
(def file (java.io.FileWriter. "example.txt"))
(.write file "Hello world!")
(.close file)
```

上記のように、```java.io.FileWriter```を使用して、テキストファイルを作成します。ファイルの内容を```.write```で指定し、```.close```を呼び出すことで、ファイルを正しくクローズすることができます。

## 深く掘り下げる
テキストファイルを書く前に、ファイルがすでに存在しているかどうかを確認する必要があります。また、ファイルを作成する際には、ファイルパスを指定することも重要です。

他の方法としては、```.append```を使用してファイルを追記したり、```.flush```を使用してファイルに書き込んだ内容を即時に保存したりすることもできます。