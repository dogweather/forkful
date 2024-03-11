---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:19.460372-07:00
description: "Bash\u3067\u306E\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u3092\u4F7F\
  \u7528\u3059\u308B\u3068\u3001\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u306B\u57FA\
  \u3065\u3044\u3066\u6587\u5B57\u5217\u3084\u30D5\u30A1\u30A4\u30EB\u3092\u691C\u7D22\
  \u3001\u64CD\u4F5C\u3001\u51E6\u7406\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u691C\u8A3C\
  \u3001\u30ED\u30B0\u30D5\u30A1\u30A4\u30EB\u306E\u89E3\u6790\u3001\u30C7\u30FC\u30BF\
  \u62BD\u51FA\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306Bregex\u3092\u4F7F\u7528\u3057\
  \u3066\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u8907\u96D1\u306A\u30C6\u30AD\u30B9\
  \u30C8\u51E6\u7406\u30CB\u30FC\u30BA\u306E\u30D1\u30BF\u30FC\u30F3\u3092\u6307\u5B9A\
  \u3059\u308B\u67D4\u8EDF\u3067\u5F37\u529B\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3059\
  \u308B\u305F\u3081\u3067\u3059\u3002"
lastmod: '2024-03-11T00:14:15.911856-06:00'
model: gpt-4-0125-preview
summary: "Bash\u3067\u306E\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u3092\u4F7F\u7528\
  \u3059\u308B\u3068\u3001\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u306B\u57FA\u3065\
  \u3044\u3066\u6587\u5B57\u5217\u3084\u30D5\u30A1\u30A4\u30EB\u3092\u691C\u7D22\u3001\
  \u64CD\u4F5C\u3001\u51E6\u7406\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u691C\u8A3C\u3001\
  \u30ED\u30B0\u30D5\u30A1\u30A4\u30EB\u306E\u89E3\u6790\u3001\u30C7\u30FC\u30BF\u62BD\
  \u51FA\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306Bregex\u3092\u4F7F\u7528\u3057\u3066\
  \u3044\u307E\u3059\u3002\u3053\u308C\u306F\u8907\u96D1\u306A\u30C6\u30AD\u30B9\u30C8\
  \u51E6\u7406\u30CB\u30FC\u30BA\u306E\u30D1\u30BF\u30FC\u30F3\u3092\u6307\u5B9A\u3059\
  \u308B\u67D4\u8EDF\u3067\u5F37\u529B\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3059\u308B\
  \u305F\u3081\u3067\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？

Bashでの正規表現（regex）を使用すると、特定のパターンに基づいて文字列やファイルを検索、操作、処理することができます。プログラマーは、入力検証、ログファイルの解析、データ抽出などのタスクにregexを使用しています。これは複雑なテキスト処理ニーズのパターンを指定する柔軟で強力な方法を提供するためです。

## どのようにして：

### 基本的なパターンマッチング
文字列がパターンに一致するかどうかを見つけるには、正規表現に一致する行を探すためのコマンドラインユーティリティである`grep`を使用できます：

```bash
echo "Hello, World!" | grep -o "World"
# 出力: World
```

### 特定のデータの抽出
regexパターンに一致するデータの部分を抽出するには、`grep`で`-o`を使用できます：

```bash
echo "Error: File not found" | grep -oE "[A-Za-z]+:"
# 出力: Error:
```

### `sed`でのRegexの使用
`sed`（ストリームエディター）はテキストの解析と変換に強力なユーティリティです。ここでは`sed`をregexと共に使用してテキストを置き換える方法です：

```bash
echo "Bash is great" | sed -e 's/great/awesome/'
# 出力: Bash is awesome
```

### 条件文でのパターンマッチング
Bashは条件文で直接regexをサポートしています：

```bash
[[ "https://example.com" =~ ^https?:// ]] && echo "URL is valid" || echo "URL is invalid"
# 出力: URL is valid
```

### `awk`での高度なパターンマッチングと操作
`awk`はCSVなどの構造化されたテキストデータを扱う際に便利な、より複雑なデータ抽出と操作をサポートする別のテキスト処理ツールです：

```bash
echo -e "ID,Name,Age\n1,John,22\n2,Jane,24" | awk -F, '$3 > 22 {print $2 "は22歳よりも年上です。"}'
# 出力: Janeは22歳よりも年上です。
```

Bashの組み込みregex機能は多くのユースケースをカバーしていますが、非常に高度なregex操作については、`perl`や`python`スクリプトとBashスクリプトを組み合わせることを検討するかもしれません。これらの言語は強力なregexライブラリ（例：Pythonの`re`）を提供しています。Pythonでの簡単な例：

```bash
echo "Capture this 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# 出力: 123
```

必要に応じてこれらのプログラミング言語を取り入れることで、Bashスクリプトでのregexの全能力を活用することができます。
