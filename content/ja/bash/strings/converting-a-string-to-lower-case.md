---
date: 2024-01-20 17:37:59.393983-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u306E\u306F\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\u5B57\u3092\u533A\u5225\u305B\
  \u305A\u306B\u51E6\u7406\u3092\u884C\u3046\u6642\u306B\u4F7F\u3044\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\u4E00\u8CAB\u6027\
  \u3092\u4FDD\u3064\u305F\u3081\u3001\u307E\u305F\u306F\u691C\u7D22\u6642\u306E\u30DF\
  \u30B9\u30DE\u30C3\u30C1\u3092\u907F\u3051\u308B\u305F\u3081\u306B\u884C\u3044\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.348927-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u306E\u306F\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\u5B57\u3092\u533A\u5225\u305B\
  \u305A\u306B\u51E6\u7406\u3092\u884C\u3046\u6642\u306B\u4F7F\u3044\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\u4E00\u8CAB\u6027\
  \u3092\u4FDD\u3064\u305F\u3081\u3001\u307E\u305F\u306F\u691C\u7D22\u6642\u306E\u30DF\
  \u30B9\u30DE\u30C3\u30C1\u3092\u907F\u3051\u308B\u305F\u3081\u306B\u884C\u3044\u307E\
  \u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

## What & Why? (何となぜ？)
文字列を小文字に変換するのは、大文字と小文字を区別せずに処理を行う時に使います。プログラマーはデータの一貫性を保つため、または検索時のミスマッチを避けるために行います。

## How to: (方法)
```Bash
# trコマンドを使用
echo "Kon'nichiwa Sekai" | tr '[:upper:]' '[:lower:]'
# 出力: kon'nichiwa sekai

# パラメータ展開を利用
str="Kon'nichiwa Sekai"
echo "${str,,}"
# 出力: kon'nichiwa sekai

# awkコマンドを使用
echo "Kon'nichiwa Sekai" | awk '{print tolower($0)}'
# 出力: kon'nichiwa sekai
```

## Deep Dive (詳細情報)
歴史的に見ると、`tr`はUnix系システムで文字列を変換する古典的なツールです。Bash バージョン 4.0 以降、パラメータ展開を使用する書き方もあります。これは `tr` に比べてサブシェルを使わないため速いです。`awk`はテキストを処理するスクリプト言語で`tolower`関数で簡単に文字列を小文字に変更できます。

選択肢として、`sed`や`perl`のコマンドラインオプションもありますが、Bash環境においては`tr`やパラメータ展開はより見かけます。実装の詳細に関しては、`tr`ではUnicodeの大文字小文字変換もサポートしており、Bashのパラメータ展開は非常に簡単に使うことができるのが特徴です。

## See Also (関連リンク)
- Bash マニュアル: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- GNU Coreutils の `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- AWK マニュアル: https://www.gnu.org/software/gawk/manual/gawk.html
