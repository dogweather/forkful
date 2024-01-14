---
title:                "Elixir: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードする理由は、インターネット上の情報を取得するためです。Elixirを使えば、そのようなタスクを簡単に実現できます。

## 手順

以下のようなコードを使用して、Elixirでウェブページをダウンロードすることができます。

```
Elixir（
  HTTPoison.get（ "https://www.google.com"）
）|
> _retorna =
> _
>結果=ライブラリ案内、レスポンスは、stderr
>レスポンス
```

このコードは、HTTPoisonというライブラリを使用し、Googleのウェブページをダウンロードしています。レスポンスはHTML形式で返され、標準エラー出力ではステータスコードなどが返されます。

## ディープダイブ

ウェブページをダウンロードするときには、得られたデータを処理することが重要です。Elixirでは、取得したデータをパースしたり、必要な情報を抽出することができます。また、並列処理やエラーハンドリングなど、コードをより柔軟にするための手段もあります。

これにより、Elixirを使用してウェブページをダウンロードすることが、より効率的かつ拡張性の高い方法であることがわかります。

## 参考

- [HTTPoisonドキュメント](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Elixir公式ウェブサイト](https://elixir-lang.org/)
- [Elixirでウェブスクレイピングを行う方法](https://www.hashnopolis.com/2019/02/08/scraping-websites-with-elixir/)