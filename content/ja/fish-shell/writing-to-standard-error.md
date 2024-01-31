---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
標準エラー出力は、エラーメッセージやプログラムの診断情報を表示するのに使います。プログラマーは、出力内容を整理して正常な出力とエラーを区別するためにこれを行います。

## How to: (方法)
Fish Shellでは、「`stderr`」を使って標準エラーに書き込みます。以下は簡単な例です。

```Fish Shell
echo "これは標準出力です。"
echo "これは標準エラーです。" >&2
```

実行すると、次のように表示されます：

```
これは標準出力です。
これは標準エラーです。
```

ただし、実際には、「これは標準エラーです。」は標準エラーを通じて出力されます。

## Deep Dive (深堀り)
元々、Unix系システムは標準出力(`stdout`)と標準エラー(`stderr`)の2つの異なる出力チャネルを提供しました。これは、出力をファイルへリダイレクトしつつエラーメッセージをユーザーに見せることができるようにするためです。Fish Shellでの実装は、これらの伝統に従い、`>`ではなく`>&`を使ってエラーをリダイレクトします。他のシェルと比べて、Fishではシンプルさと直感性を重視しています。

## See Also (関連情報)
- Fish Shellの公式ドキュメント： https://fishshell.com/docs/current/
- Unixの標準ストリームについての詳細： https://ja.wikipedia.org/wiki/標準ストリーム
