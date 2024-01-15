---
title:                "ウェブページをダウンロードする"
html_title:           "Bash: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードする理由はさまざまですが、最も一般的な理由はオフラインでページを閲覧したい場合や特定のデータを取得したい場合です。

## 使い方

ウェブページをダウンロードするには、Bashの`curl`コマンドを使用します。サンプルコードは以下のようになります。

```Bash
curl <URL>
```

コマンドには様々なオプションがありますが、基本的な使い方は上記の通りです。実際にコマンドを実行すると、ダウンロードされたファイルやHTMLコードがターミナルに表示されます。ファイルを指定することで、デフォルトでは`curl`はファイルをダウンロードしません。詳細なオプションやカスタマイズ方法については、公式ドキュメントを参照しましょう。

## ディープダイブ

`curl`はウェブページをダウンロードするだけでなく、様々な用途にも使用できます。例えば、APIからデータを取得したり、ファイルをアップロードしたりすることも可能です。しかし、セキュリティに関する注意点もあります。ウェブページのダウンロードには、信頼できるサイトからのみ行うようにしましょう。

## 参考リンク

- Bash 公式ドキュメント (https://www.gnu.org/software/bash/)
- `curl`コマンドのマニュアル (https://curl.se/)
- `curl`を使用したAPIのデータ取得チュートリアル (https://www.hostinger.jp/tutorials/curl/)