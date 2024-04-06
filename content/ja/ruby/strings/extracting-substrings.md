---
date: 2024-01-20 17:46:31.677926-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:55.011421-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u306E\u62BD\
  \u51FA\u306FRuby\u304C\u767B\u5834\u3057\u305F1995\u5E74\u304B\u3089\u5B58\u5728\
  \u3057\u3066\u3044\u307E\u3059\u3002`slice`\u30E1\u30BD\u30C3\u30C9\u3084`[]`\u30AA\
  \u30DA\u30EC\u30FC\u30BF\u3092\u4F7F\u3063\u3066\u6587\u5B57\u5217\u304B\u3089\u90E8\
  \u5206\u6587\u5B57\u5217\u3092\u53D6\u308A\u51FA\u305B\u307E\u3059\u3002\u3053\u308C\
  \u3089\u306F\u5185\u90E8\u7684\u306B\u540C\u3058\u6A5F\u69CB\u3092\u4F7F\u7528\u3057\
  \u3066\u3044\u307E\u3059\u3002\u6B63\u898F\u8868\u73FE\u306F\u5F37\u529B\u306A\u30D1\
  \u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u3092\u63D0\u4F9B\u3057\u3001\u8907\
  \u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u306E\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\
  \u3082\u62BD\u51FA\u3067\u304D\u307E\u3059\u3002`slice`\u30E1\u30BD\u30C3\u30C9\u306F\
  `slice!`\u30D0\u30FC\u30B8\u30E7\u30F3\u3082\u3042\u308A\u3001\u3053\u308C\u3092\
  \u4F7F\u7528\u3059\u308B\u3068\u5143\u306E\u6587\u5B57\u5217\u304B\u3089\u30B5\u30D6\
  \u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u524A\u9664\u3067\u304D\u307E\u3059\u3002\u307E\
  \u305F\u3001\u7BC4\u56F2\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u4F7F\u3063\u3066\
  \u8907\u6570\u306E\u6587\u5B57\u3092\u53D6\u308A\u51FA\u3059\u65B9\u6CD5\u3082\u4FBF\
  \u5229\u3067\u3001\u3088\u308A\u76F4\u611F\u7684\u306A\u64CD\u4F5C\u304C\u53EF\u80FD\
  \u3067\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to: (やり方)
```Ruby
# 与えられた文字列からサブストリングを取り出す基本的な例
str = "こんにちは、Rubyの世界へ！"
# 文字位置を指定して取り出す (位置5から始まる3文字)
substring = str[5, 3]
puts substring  # => "は、R"

# 文字列の範囲を使って取り出す (位置7から位置12まで)
substring_range = str[7..12]
puts substring_range  # => "Rubyの"

# 正規表現を使って取り出す
regex_substring = str[/[あ-ん]+/]
puts regex_substring  # => "こんにちは"
```

## Deep Dive (詳細情報)
サブストリングの抽出はRubyが登場した1995年から存在しています。`slice`メソッドや`[]`オペレータを使って文字列から部分文字列を取り出せます。これらは内部的に同じ機構を使用しています。正規表現は強力なパターンマッチングを提供し、複雑なパターンのサブストリングも抽出できます。`slice`メソッドは`slice!`バージョンもあり、これを使用すると元の文字列からサブストリングを削除できます。また、範囲オブジェクトを使って複数の文字を取り出す方法も便利で、より直感的な操作が可能です。

## See Also (参照)
- Rubyの公式ドキュメントの `String` クラス: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)
- Ruby正規表現ガイド: [http://www.ruby-doc.org/core-2.7.1/Regexp.html](http://www.ruby-doc.org/core-2.7.1/Regexp.html)
