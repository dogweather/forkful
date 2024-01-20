---
title:                "HTMLの解析"
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/parsing-html.md"
---

{{< edit_this_page >}}

---

## 1.何となぜ?

HTMLパースとは、HTML文書のデータ構造を解析して操作しやすい形式に変換することです。プログラマーは、ウェブスクレイピングやウェブコンテンツの自動化操作などに使用するデータを抽出するためにこれを行います。

---

## 2.やり方:

以下は、Bashを使ったHTMLパージングの基本例です：

```Bash 
$ echo "<h1>こんにちは、世界！</h1>" | grep -oP '(?<=<h1>).*(?=</h1>)'
```

このコードは出力に`こんにちは、世界！`を返します。

しっかりとしたHTML解析には、xsoupやpupのような専用のツールを使うことを推奨します。

---

## 3.深掘り:

1. 歴史的な文脈：

      Bashは元々1971年にはじめて実装されたsh（Bourne Shell）の後継として1990年代に生み出されました。  
   
2. 代替手段：

      Bash以外にも、PythonのBeautifulSoupやRubyのNokogiriなど、他の言語のライブラリを用いることでより強固なHTMLパージングが可能です。
   
3. 実装詳細：

      BashでのHTMLパージングは、基本的には正規表現を使用しています。しかし複雑なパージングには向かず、その場合は目的に合わせたツールの利用を推奨します。

---

## 4. 詳細資料:

- Bash Scripting Tutorial: [https://ryanstutorials.net/bash-scripting-tutorial/](https://ryanstutorials.net/bash-scripting-tutorial/)
   
- xsoup: [https://github.com/code4craft/xsoup](https://github.com/code4craft/xsoup)

- pup: [https://github.com/ericchiang/pup](https://github.com/ericchiang/pup)
   
- BeautifulSoup: [https://www.crummy.com/software/BeautifulSoup/](https://www.crummy.com/software/BeautifulSoup/)
   
- Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)

---