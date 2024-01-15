---
title:                "HTMLパース"
html_title:           "Fish Shell: HTMLパース"
simple_title:         "HTMLパース"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ

HTMLの解析に、なぜ取り組むのでしょうか？あなたがプログラマーであるなら、大きなウェブサイトの内容をスクレイピングしたり、特定の情報を抽出したりすることがあるかもしれません。また、ウェブアプリケーションを開発する際にも必要になるかもしれません。どのような理由であっても、HTMLの解析は非常に有用なスキルです。

## やり方

では、Fish Shellを使ってHTMLを解析する方法を見ていきましょう。まずは、以下のようにFishのセッションを開始しましょう。

```Fish Shell
fish
```

次に、HTMLを解析するために必要なパッケージをインストールします。

```Fish Shell
apt-get install html-xml-utils
```

今回は、Wikipediaの日本語版トップページから記事のタイトルを抽出する例を示します。

```Fish Shell
curl -s https://ja.wikipedia.org/wiki/%E3%83%A1%E3%82%A4%E3%83%B3%E3%83%9A%E3%83%BC%E3%82%B8 | hxnormalize -x | hxselect -c "div#mw-content-text > div.mw-parser-output > ul li > a"
```

実行すると、以下のような出力が得られます。

```
メインページ
2020東京オリンピック
提供依頼
宣言
経済学者
クリス・リズカ
広韻
勾践
エレナ・パン
...
```

これで、Wikipediaのトップページから抽出した記事のタイトルを取得できました。

## ディープダイブ

今回紹介した例では、curlコマンドでHTMLを取得し、それをhtml-xml-utilsパッケージを使ってパースしていました。また、hxselectコマンドではCSSセレクターを使って特定の要素を抽出していました。これらのコマンドはFish Shellだけでなく、他のシェルでも利用できるので、HTMLの解析には非常に便利です。

ただし、HTMLは常に変化しているため、解析には注意が必要です。特定のサイトに依存するコードを書くと、サイトの構造が変更された際にうまく動作しなくなってしまいます。そのため、柔軟に対応できるように、コードを書く際はできるだけ一般的な手法を使うように心がけましょう。

## 参考リンク

- [Fish Shell公式サイト](https://fishshell.com/)
- [HTML/XMLユーティリティパッケージのインストール方法](https://www.debian.org/distrib/packages.ja.html)
- [Wikipediaの日本語版トップページ](https://ja.wikipedia.org/wiki/%E3%83%A1%E3%82%A4%E3%83%B3%E3%83%9A%E3%83%BC%E3%82%B8)