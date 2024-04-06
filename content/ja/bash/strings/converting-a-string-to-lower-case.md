---
date: 2024-01-20 17:37:59.393983-07:00
description: "How to: (\u65B9\u6CD5) \u6B74\u53F2\u7684\u306B\u898B\u308B\u3068\u3001\
  `tr`\u306FUnix\u7CFB\u30B7\u30B9\u30C6\u30E0\u3067\u6587\u5B57\u5217\u3092\u5909\
  \u63DB\u3059\u308B\u53E4\u5178\u7684\u306A\u30C4\u30FC\u30EB\u3067\u3059\u3002Bash\
  \ \u30D0\u30FC\u30B8\u30E7\u30F3 4.0 \u4EE5\u964D\u3001\u30D1\u30E9\u30E1\u30FC\u30BF\
  \u5C55\u958B\u3092\u4F7F\u7528\u3059\u308B\u66F8\u304D\u65B9\u3082\u3042\u308A\u307E\
  \u3059\u3002\u3053\u308C\u306F `tr`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.187870-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6B74\u53F2\u7684\u306B\u898B\u308B\u3068\u3001`tr`\u306F\
  Unix\u7CFB\u30B7\u30B9\u30C6\u30E0\u3067\u6587\u5B57\u5217\u3092\u5909\u63DB\u3059\
  \u308B\u53E4\u5178\u7684\u306A\u30C4\u30FC\u30EB\u3067\u3059\u3002Bash \u30D0\u30FC\
  \u30B8\u30E7\u30F3 4.0 \u4EE5\u964D\u3001\u30D1\u30E9\u30E1\u30FC\u30BF\u5C55\u958B\
  \u3092\u4F7F\u7528\u3059\u308B\u66F8\u304D\u65B9\u3082\u3042\u308A\u307E\u3059\u3002\
  \u3053\u308C\u306F `tr` \u306B\u6BD4\u3079\u3066\u30B5\u30D6\u30B7\u30A7\u30EB\u3092\
  \u4F7F\u308F\u306A\u3044\u305F\u3081\u901F\u3044\u3067\u3059\u3002`awk`\u306F\u30C6\
  \u30AD\u30B9\u30C8\u3092\u51E6\u7406\u3059\u308B\u30B9\u30AF\u30EA\u30D7\u30C8\u8A00\
  \u8A9E\u3067`tolower`\u95A2\u6570\u3067\u7C21\u5358\u306B\u6587\u5B57\u5217\u3092\
  \u5C0F\u6587\u5B57\u306B\u5909\u66F4\u3067\u304D\u307E\u3059."
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
