---
date: 2024-01-20 17:42:13.581501-07:00
description: "How to (\u3084\u308A\u65B9) Fish Shell\u306B\u304A\u3051\u308B\u30D1\
  \u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u306F\u3001\u6B63\u898F\u8868\u73FE\u3092\u7528\
  \u3044\u3066\u67D4\u8EDF\u306A\u6587\u5B57\u5217\u64CD\u4F5C\u3092\u53EF\u80FD\u306B\
  \u3057\u307E\u3059\u3002\u4F1D\u7D71\u7684\u306AUnix\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.199122-06:00'
model: gpt-4-1106-preview
summary: "How to (\u3084\u308A\u65B9) Fish Shell\u306B\u304A\u3051\u308B\u30D1\u30BF\
  \u30FC\u30F3\u30DE\u30C3\u30C1\u306F\u3001\u6B63\u898F\u8868\u73FE\u3092\u7528\u3044\
  \u3066\u67D4\u8EDF\u306A\u6587\u5B57\u5217\u64CD\u4F5C\u3092\u53EF\u80FD\u306B\u3057\
  \u307E\u3059\u3002\u4F1D\u7D71\u7684\u306AUnix Shell\u3068\u6BD4\u8F03\u3057\u3066\
  \u3001\u30B7\u30F3\u30BF\u30C3\u30AF\u30B9\u304C\u76F4\u611F\u7684\u3067\u3001\u72EC\
  \u81EA\u306E\u30B3\u30DE\u30F3\u30C9\u3092\u4F7F\u3046\u3053\u3068\u3067\u8907\u96D1\
  \u306A\u30D1\u30BF\u30FC\u30F3\u3082\u5BB9\u6613\u306B\u6271\u3048\u307E\u3059\u3002\
  \u4ED6\u306E\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C6\u30A3\u30F3\u30B0\u8A00\
  \u8A9E\u3068\u6BD4\u8F03\u3057\u3066\u3001Fish\u306F\u30E6\u30FC\u30B6\u30FC\u30D5\
  \u30EC\u30F3\u30C9\u30EA\u30FC\u3067\u30B9\u30AF\u30EA\u30D7\u30C8\u304C\u8AAD\u307F\
  \u3084\u3059\u304F\u306A\u308B\u3088\u3046\u8A2D\u8A08\u3055\u308C\u3066\u3044\u307E\
  \u3059\u304C\u3001\u4E00\u90E8POSIX\u4E92\u63DB\u3067\u306A\u3044\u3053\u3068\u304B\
  \u3089\u4ED6\u306E\u30B7\u30A7\u30EB\u3068\u306E\u79FB\u884C\u306B\u306F\u6CE8\u610F\
  \u304C\u5FC5\u8981\u3067\u3059\u3002`string`\u30B3\u30DE\u30F3\u30C9\u306FFish Shell\u3067\
  \u8FFD\u52A0\u3055\u308C\u305F\u6A5F\u80FD\u3067\u3001sed\u3084awk\u3068\u3044\u3063\
  \u305F\u5F93\u6765\u306EUnix\u30B3\u30DE\u30F3\u30C9\u306B\u4EE3\u308F\u308B\u9078\
  \u629E\u80A2\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u308C\u306B\
  \u3088\u308A\u3001\u30D1\u30A4\u30D7\u30E9\u30A4\u30F3\u3092\u901A\u3057\u3066\u52B9\
  \u7387\u7684\u306B\u6587\u5B57\u5217\u51E6\u7406\u3092\u884C\u3048\u308B\u3088\u3046\
  \u306B\u306A\u308A\u307E\u3059\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to (やり方)
```Fish Shell
# 文字列からパターンに一致する部分を削除する
echo "fish_shell_rocks" | string replace -r '_.*' ''
# 出力: fish

# 複数のパターンにマッチする文字を削除
echo "remove vowels from this sentence." | string replace -a 'a' '' | string replace -a 'e' '' | string replace -a 'i' '' | string replace -a 'o' '' | string replace -a 'u' ''
# 出力: rmv vwls frm ths sntnc.

# 特定のファイルからパターンにマッチする行を削除
string match -v "pattern" < file.txt > new_file.txt
# 'file.txt'から'pattern'に一致する行を削除して、'new_file.txt'に保存します。
```

## Deep Dive (深掘り)
Fish Shellにおけるパターンマッチは、正規表現を用いて柔軟な文字列操作を可能にします。伝統的なUnix Shellと比較して、シンタックスが直感的で、独自のコマンドを使うことで複雑なパターンも容易に扱えます。他のシェルスクリプティング言語と比較して、Fishはユーザーフレンドリーでスクリプトが読みやすくなるよう設計されていますが、一部POSIX互換でないことから他のシェルとの移行には注意が必要です。`string`コマンドはFish Shellで追加された機能で、sedやawkといった従来のUnixコマンドに代わる選択肢を提供しています。これにより、パイプラインを通して効率的に文字列処理を行えるようになります。

## See Also (関連情報)
- [Fish Shell Documentation - String](https://fishshell.com/docs/current/commands.html#string)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Regular Expressions in Fish](https://fishshell.com/docs/current/index.html#regexp)
