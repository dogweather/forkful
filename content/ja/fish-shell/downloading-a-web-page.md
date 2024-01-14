---
title:                "Fish Shell: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードする理由は様々ですが、よくあるのはウェブコンテンツをオフラインで閲覧するためです。また、ウェブスクレイピングのためにウェブページをダウンロードすることもあります。どのような理由であっても、ダウンロードはウェブ開発やデータ収集において重要なスキルです。

## 手順

Fish Shellを使用してウェブページをダウンロードする方法は簡単です。まず、`curl`コマンドを使用してウェブページのURLを指定します。次に、`-o`フラグを使用してファイル名を指定します。最後に、`-L`フラグを使用してリダイレクトを解決します。

```
curl https://example.com -o sample.html -L
```

以上のコマンドを実行すると、`sample.html`という名前のファイルが現在のディレクトリにダウンロードされます。

## 深く掘り下げる

ウェブページをダウンロードする際は、さまざまなオプションを使用することができます。例えば、`-A`フラグを使用すると、ダウンロードするファイルのユーザーエージェントを設定することができます。これは、ダウンロードするファイルが特定のブラウザからのアクセスであるかのように見せることができます。

また、`--limit-rate`フラグを使用すると、ダウンロード速度を制限することができます。これは、大量のファイルをダウンロードする場合に便利です。

さらに、ダウンロードしたいウェブページがログインが必要な場合は、`-u`フラグを使用してユーザー名とパスワードを指定することができます。このように、`curl`コマンドは様々なシナリオに対応するための多彩なオプションを提供しています。

## 参考リンク

- [Fish Shell公式サイト](https://fishshell.com/)
- [curlの公式ドキュメント](https://curl.se/docs/)
- [ウェブスクレイピングの入門ガイド](https://medium.com/@jimlinden/5-step-guide-to-web-scraping-with-bash-c7011c3658f6)