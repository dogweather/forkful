---
aliases:
- /ja/python/deleting-characters-matching-a-pattern/
date: 2024-01-20 17:43:19.656117-07:00
description: "\u6587\u5B57\u30D1\u30BF\u30FC\u30F3\u306B\u30DE\u30C3\u30C1\u3059\u308B\
  \u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3053\u3068\u306F\u3001\u4E0D\u8981\u306A\
  \u30C7\u30FC\u30BF\u3092\u53D6\u308A\u9664\u304D\u3001\u6587\u5B57\u5217\u3092\u7279\
  \u5B9A\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306B\u6574\u3048\u308B\u51E6\u7406\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u30C7\u30FC\u30BF\u306E\
  \u691C\u8A3C\u3084\u524D\u51E6\u7406\u3067\u3053\u308C\u3092\u5B9F\u884C\u3057\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.549011
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u30D1\u30BF\u30FC\u30F3\u306B\u30DE\u30C3\u30C1\u3059\u308B\
  \u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3053\u3068\u306F\u3001\u4E0D\u8981\u306A\
  \u30C7\u30FC\u30BF\u3092\u53D6\u308A\u9664\u304D\u3001\u6587\u5B57\u5217\u3092\u7279\
  \u5B9A\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306B\u6574\u3048\u308B\u51E6\u7406\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u30C7\u30FC\u30BF\u306E\
  \u691C\u8A3C\u3084\u524D\u51E6\u7406\u3067\u3053\u308C\u3092\u5B9F\u884C\u3057\u307E\
  \u3059\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字パターンにマッチする文字を削除することは、不要なデータを取り除き、文字列を特定のフォーマットに整える処理です。プログラマは、データの検証や前処理でこれを実行します。

## How to (方法):
```Python
import re

# 文字列の例
original_string = "123-456-7890"

# パターンにマッチする文字を削除する
cleaned_string = re.sub(r'-', '', original_string)  # ハイフンを削除

print(cleaned_string)  # 出力: 1234567890
```

## Deep Dive (詳細な潜水):
パターンマッチで文字を削除する機能は、Perl言語の強力な正規表現の影響を受けてPythonにも実装されました。`re.sub()` 関数は正規表現を利用して柔軟な文字削除を可能にします。文字以外にも、特定のパターンを持つ部分文字列の置換や削除もできます。他の方法としては、`str.replace()`や文字列メソッドの組み合わせがありますが、正規表現はより複雑なパターンに対応しています。

```Python
# str.replace() を使った例
simple_string = "foobar"
modified_string = simple_string.replace("o", "")  # 'o' を削除
print(modified_string)  # 出力: fbar
```

正規表現の使用は、実行速度が比較的遅いため、パフォーマンスが重要な場面では注意が必要です。ただし、その強力さと柔軟性から、データクレンジングやテキスト処理では頻繁に用いられます。

## See Also (関連項目):
- Python公式ドキュメントの `re` モジュール: https://docs.python.org/3/library/re.html
- 正規表現についての追加情報: https://www.regular-expressions.info/
- `str.replace` メソッドのドキュメント: https://docs.python.org/3/library/stdtypes.html#str.replace
- 文字列操作に関するPythonチュートリアル: https://docs.python.org/3/tutorial/introduction.html#strings
