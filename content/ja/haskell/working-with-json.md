---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
JSONはデータ交換のフォーマット。Web APIとの対話や設定ファイル利用のため重要。Haskellで扱う理由は型安全な操作とパフォーマンス。

## How to: (やり方)
```Haskell
-- 必要なパッケージ：aeson
import Data.Aeson

-- JSONエンコード
encodeExample = encode (["apple", "banana"] :: [String])

-- JSONデコード
decodeExample = decode "[\"apple\",\"banana\"]" :: Maybe [String]

main = do
  -- エンコードした結果を表示
  print encodeExample
  -- デコードした結果を表示
  print decodeExample
```

出力:
```
"[\"apple\",\"banana\"]"
Just ["apple","banana"]
```

## Deep Dive (深掘り)
JSONはJavaScript Object Notationの略。2001年に開発され、軽量なテキストベースの交換フォーマットとして広まった。Haskellの`aeson`ライブラリはJSONのパースと生成における標準的選択。C言語で書かれた`json`ライブラリに比べ速度と安全性に優れる。代わりに`yaml`や`xml`など利用可能だが、JSONはシンプルでWebとの相性が良い。

## See Also (関連リンク)
- `aeson` パッケージ: https://hackage.haskell.org/package/aeson
- JSON specification: https://www.json.org/json-ja.html
- Real World HaskellのJSON章: http://book.realworldhaskell.org/read/json.html
