---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ？
ウェブページのダウンロードとは、インターネット上のページの内容を自分のコンピュータにコピーすることです。プログラマーは、情報分析やデータスクレイピングの目的でこれを行います。

## 方法:
Elixirの`HTTPoison`ライブラリを使えば、ウェブページをダウンロードするのは簡単です。まずはライブラリをインストールしましょう。

```Elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

その後に、以下のようなコードを使用してウェブページを取得します。

```Elixir
defmodule Downloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, response} -> IO.puts response.body
      {:error, reason} -> IO.puts "Error: #{reason}"
    end
  end
end
```
## Deep Dive
ウェブページのダウンロードは、ウェブスクレイピングの一部として以前から行われていました。Elixirの前にはPerlやPython等の言語が使われてきました。しかし、Elixirはそのマルチコア対応による強力な並行処理能力やイマーチャンス・オブジェクトの扱いやすさで一歩リードしています。

代替手段としては、他のライブラリを使う方法もあります。例えば`Mint`や`Tesla`などがあります。しかし、簡単な仕事では比較的シンプルで使いやすい`HTTPoison`をお勧めします。

## 参照
以下のリンクでより詳しい情報を得ることができます:

- Elixir 公式ドキュメンテーション: https://elixir-lang.org/docs.html
- HTTPoison Github ページ: https://github.com/edgurgel/httpoison
- Elixir School (HTTP): https://elixirschool.com/ja/lessons/libraries/httpoison/