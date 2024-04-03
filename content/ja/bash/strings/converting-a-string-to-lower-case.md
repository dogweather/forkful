---
date: 2024-01-20 17:37:59.393983-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.348927-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
