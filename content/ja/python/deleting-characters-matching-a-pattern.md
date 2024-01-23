---
title:                "パターンに一致する文字を削除する"
date:                  2024-01-20T17:43:19.656117-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/deleting-characters-matching-a-pattern.md"
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
