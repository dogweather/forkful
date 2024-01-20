---
title:                "「JSON での作業」"
html_title:           "Haskell: 「JSON での作業」"
simple_title:         "「JSON での作業」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## 何それ？なんでやるの？

JSONを扱うことは、データの受け渡しや保存に便利なフォーマットです。JSONは、人間にとって読みやすく、コンピュータにとっても解析しやすい形式を持っています。プログラマーたちは、データを簡単に処理できるようにするために、JSONを使用します。

## 方法：

Haskellでは、JSONを扱うための便利なライブラリがたくさんあります。以下のようにインポートして、簡単にJSONの値を作成できます。

```Haskell
import Data.Aeson
```

また、ファイルからJSONを読み込んだり、JSONからファイルを生成したりすることもできます。

```Haskell
import qualified Data.ByteString.Lazy as BS
import Data.Aeson.Encode.Pretty

-- JSONからファイルを生成
writeJSONtoFile :: ToJSON a => FilePath -> a -> IO ()
writeJSONtoFile path json =
  BS.writeFile path $ encodePretty json

-- ファイルからJSONを読み込み
readJSONfromFile :: FromJSON a => FilePath -> IO (Maybe a)
readJSONfromFile path = do
  fileContent <- BS.readFile path
  return $ decode fileContent
```

## 詳しく知る：

JSONは、プログラミング言語に依存しない一般的なフォーマットです。特に、ウェブアプリケーションを開発する際に、サーバーとクライアント間のデータのやり取りに頻繁に使用されます。他の言語でも、JSONを扱うためのライブラリが多数存在しますが、Haskellの型システムのおかげで、静的型付けと安全なコードを実現することができます。

## 関連情報を見る：

- [Hackage: Data.Aeson](https://hackage.haskell.org/package/aeson)
- [JSON公式サイト](https://www.json.org/json-ja.html)