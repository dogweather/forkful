---
title:                "Fish Shell: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

# なぜHTMLパースを行うのか？

Webアプリケーションやスクレイピングなど様々な場面でHTMLソースコードを扱う必要があります。しかし、直接HTMLを読み込むことは非常に煩雑で、情報を抽出するのも困難です。ここでFish Shellを使ったHTMLパースが役立ちます。HTMLパースを行うことで、HTMLソースコードから必要な情報を簡単に抽出することができるようになります。

## HTMLパースのやり方

Fish Shellには、HTMLパースを簡単に行うためのモジュールが用意されています。まずはそのモジュールをインストールしましょう。

```Fish Shell
sudo apt-get install html-xml-utils
```

次に、HTMLソースコードを取得します。例えば、以下のようなHTMLソースコードがあるとします。

```html
<html>
<head>
<title>Fish Shell</title>
</head>
<body>
<h1>Welcome to Fish Shell</h1>
<p>This is a Fish Shell tutorial for parsing HTML.</p>
<ul>
<li>Easy to use</li>
<li>Efficient</li>
<li>Powerful</li>
</ul>
</body>
</html>
```

この場合、タイトルとリストの項目を抽出したいとします。それを行うために、以下のようにコマンドを実行します。

```Fish Shell
hxselect -s "\n" -c "title" sample.html
```

このコマンドによって、"Fish Shell"というタイトルが抽出されます。hxselectは、CSSのセレクタを使ってHTMLから要素を抽出することができるツールです。`-s "\n"`オプションをつけることで、抽出した要素を改行区切りで出力するようになります。`-c`オプションをつけることで、要素の内容のみを出力するようになります。

次に、リストの項目を抽出するために、以下のようなコマンドを実行します。

```Fish Shell
hxselect -s "\n" -c "ul li" sample.html
```

これによって、リストの項目が改行区切りで出力されます。同様に、要素の内容のみを出力したい場合は`-c`オプションをつけることで実現できます。

## HTMLパースの深層

上記の例では、タイトルとリストの項目を抽出するだけでしたが、実際にはさらに多くの要素を抽出することができます。例えば、指定した属性やさらに奥の階層にある要素も抽出できます。

また、Fish Shellにはパイプラインという仕組みがあり、複数のコマンドを組み合わせることで、より柔軟なHTMLパースが可能になります。さまざまなオプションやコマンドを試してみることで、さらに高度なHTMLパースを行うことができるようになります。

# See Also

- [HTML-XML-utils ドキュメンテーション](https://www.w3.org/Tools/HTML-XML-utils/)
- [Fish Shell マニュアル](https://fishshell.com/docs/current/index.html)
- [CSSセレクタの使い方](https://coderkan.com/css-selector/)