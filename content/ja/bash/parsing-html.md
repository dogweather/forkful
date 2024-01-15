---
title:                "「HTMLの解析」"
html_title:           "Bash: 「HTMLの解析」"
simple_title:         "「HTMLの解析」"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ

HTMLをパースすることに関わる理由は、ウェブスクレイピングやデータ収集に役立つからです。HTMLは、ウェブサイトの情報を構造化した形式で提供しますので、パースすることで必要なデータを取得しやすくなります。

## 方法

### 1. curlコマンドを使用してHTMLを取得する

HTMLを取得するためには、まずウェブサイトのURLを使用して`curl`コマンドを実行します。例えば、GoogleのホームページからHTMLを取得するには、以下のコマンドを実行します。

```Bash
curl https://www.google.com
```

#### 出力例

以下のようなHTMLが取得できます。

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Google</title>
  </head>
  <body>
    <h1>Hello World!</h1>
  </body>
</html>
```

### 2. grepコマンドを使用して特定の要素を抽出する

`grep`コマンドを使用すると、特定の文字列やパターンを含む行を抽出することができます。例えば、`<h1>`タグの中身を抽出するには以下のようなコマンドを使用します。

```Bash
grep "<h1>" index.html
```

#### 出力例

```
<h1>Hello World!</h1>
```

### 3. sedコマンドを使用してHTMLを整形する

`sed`コマンドを使用すると、テキストを検索・置換したり、特定の行を削除したりすることができます。例えば、不要な`<h1>`タグを削除してテキストのみを抽出するには以下のようなコマンドを使用します。

```Bash
sed "s/<[^>]*>//g" index.html
```

#### 出力例

```
Hello World!
```

## 深堀り

パースとは、テキストやデータを解析して構造化することを指します。HTMLをパースすることで、ウェブサイトのタグやテキストを分析し、必要な情報を取得することができます。また、パースしたデータをデータベースに保存することで、自動的にデータを更新したり処理を行ったりすることもできます。

## 参考リンク

- [curlコマンドの使用方法](https://curl.haxx.se/docs/manpage.html)
- [grepコマンドの使用方法](https://www.gnu.org/software/grep/manual/grep.html)
- [sedコマンドの使用方法](https://www.gnu.org/software/sed/manual/sed.html)