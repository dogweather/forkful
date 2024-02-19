---
aliases:
- /ja/bash/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:25.981206-07:00
description: "Bash\u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\
  \u306E\u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u3084\u91CD\u8981\u306A\u8A3A\u65AD\u51FA\u529B\u3092\u6A19\u6E96\u51FA\u529B\
  \uFF08stdout\uFF09\u3068\u306F\u5225\u306B\u3059\u308B\u3053\u3068\u3092\u610F\u5473\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\
  \u884C\u3046\u3053\u3068\u3067\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3092\u7C21\u5358\u306B\u8B58\u5225\u3001\u8A18\u9332\u3001\u307E\u305F\u306F\u7121\
  \u8996\u3057\u3084\u3059\u304F\u3057\u3001\u30C7\u30D0\u30C3\u30B0\u3084\u30ED\u30B0\
  \u306E\u51E6\u7406\u3092\u652F\u63F4\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.089475
model: gpt-4-0125-preview
summary: "Bash\u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\
  \u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3084\u91CD\u8981\u306A\u8A3A\u65AD\u51FA\u529B\u3092\u6A19\u6E96\u51FA\u529B\uFF08\
  stdout\uFF09\u3068\u306F\u5225\u306B\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\
  \u3046\u3053\u3068\u3067\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\
  \u7C21\u5358\u306B\u8B58\u5225\u3001\u8A18\u9332\u3001\u307E\u305F\u306F\u7121\u8996\
  \u3057\u3084\u3059\u304F\u3057\u3001\u30C7\u30D0\u30C3\u30B0\u3084\u30ED\u30B0\u306E\
  \u51E6\u7406\u3092\u652F\u63F4\u3057\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？
Bashでの標準エラー（stderr）への書き込みは、エラーメッセージや重要な診断出力を標準出力（stdout）とは別にすることを意味します。プログラマーはこれを行うことで、エラーメッセージを簡単に識別、記録、または無視しやすくし、デバッグやログの処理を支援します。

## 方法:
Bashでは、`>&2` を使用して出力をstderrにリダイレクトします。基本的な例を以下に示します：

```bash
echo "This is a normal message"
echo "This is an error message" >&2
```

このスクリプトを実行すると、両方のメッセージがコンソールに表示されますが、リダイレクトすると、stdoutとstderrを分離できます。例えば：

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt`は`"This is a normal message"`を含み、`error.txt`は`"This is an error message"`をキャプチャします。

実用的な使用例として、ファイルを処理し、ファイルが存在しない場合にエラーを報告するスクリプトを考えてみましょう：

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename does not exist!" >&2
    exit 1
else
    echo "Processing $filename"
fi
```

`example.txt`が存在しない場合、コンソールでのサンプル出力は以下の通りです：

```
example.txt does not exist!
```

Bashではstderrを処理するための直接のサードパーティーライブラリはありませんが、リダイレクションはネイティブにサポートされており、一般的には十分です。しかし、複雑なアプリケーションの場合、`syslog`や`log4bash`のようなログフレームワークや外部ログツールを組み込んで、stdoutとstderrをより効果的に管理することができます。
