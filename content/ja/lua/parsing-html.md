---
title:                "HTML の解析"
html_title:           "Lua: HTML の解析"
simple_title:         "HTML の解析"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/parsing-html.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

HTMLパーサリングとは何か？プログラマーがそれをする理由は？

HTMLパーサリングとは、HTMLドキュメントを構造化されたデータに変換することです。プログラマーはこれを行うことで、Webサイトやアプリケーションのコンテンツを自動的に取得したり、特定の要素を抽出したりすることができます。

## 方法：

Luaを使ってHTMLパーサリングを行う方法を見ていきましょう。まず、タグを含むHTML文字列を用意します。次に、Luaの ```string.match()``` 関数を使用して、パターンを指定してタグ内のテキストを取得します。最後に、必要に応じて取得したテキストを処理することができます。

例えば、以下のようなHTML文字列があったとします。

```
<html>
	<head>
		<title>Hello World</title>
	</head>
	<body>
		<h1>Welcome to my website!</h1>
		<p>This is a sample paragraph.</p>
	</body>
</html>
```

この文字列から、タイトルと本文を取得するには、次のようなコードを使用します。

```lua
local html_string = "<html><head><title>Hello World</title></head><body><h1>Welcome to my website!</h1><p>This is a sample paragraph.</p></body></html>"

local title = string.match(html_string, "<title>(.-)</title>")
local paragraph = string.match(html_string, "<p>(.-)</p>")

print(title)
--> Hello World

print(paragraph)
--> This is a sample paragraph.
```

このように、HTMLパーサリングは簡単に実装することができます。

## ディープダイブ：

HTMLパーサリングの歴史や代替方法について深く掘り下げてみましょう。HTMLパーサリングは最初、手動で行われていましたが、Webの発展とともに自動化されるようになりました。現在では、ほとんどのプログラミング言語でHTMLパーサリングを行うことができますが、Luaのようにシンプルな言語でも十分実装することができます。

## 関連リンク：

- [Luaの公式ウェブサイト](https://www.lua.org/ja/)
- [HTMLパーサリングの歴史](https://en.wikipedia.org/wiki/HTML_parsing)
- [他のプログラミング言語を使ったHTMLパーサリングの方法](https://www.w3schools.com/jquery/ajax_load.asp)