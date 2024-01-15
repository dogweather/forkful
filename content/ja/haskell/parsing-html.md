---
title:                "「HTMLの解析」"
html_title:           "Haskell: 「HTMLの解析」"
simple_title:         "「HTMLの解析」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## なぜパースするのか

パースはプログラミングにおいて、特にWeb開発において重要な機能です。HTMLという言語は非常に人間にとってわかりやすいものですが、コンピューターにとっては理解しにくいため、パースを行うことでコンピューターでも理解できる形式に変換する必要があります。

## パースの方法

まず、HaskellでHTMLをパースするためには、**html-conduit**というライブラリを使用する必要があります。次に、**http-conduit**を使用してHTMLを取得し、**select**関数を用いて特定の要素を抽出してパースすることができます。

```Haskell
import Control.Monad (void)
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument, ($//), (&//), element, content)

main :: IO ()
main = do
    -- HTMLを取得
    doc <- simpleHttp "https://example.com"

    -- 全てのpタグの中身を取得
    let cursor = fromDocument $ parseLBS doc
        paragraphs = cursor $// element "p" &// content

    -- 結果の表示
    mapM_ putStrLn paragraphs
```

上記のコードでは、**mapM_**関数を使用して抽出した要素を一つずつ表示しています。また、**&//**を用いることで、pタグのように特定の要素をネストして抽出することもできます。

### 出力例

```
テキスト
テキスト
画像
```

## 深堀り

前述したように、HTMLは人間にとってわかりやすい言語ですが、コンピューターにとっては扱いづらいものです。そのため、パースを行うことで、特定の要素を抽出するだけではなく、データを加工したり、データベースに保存したりすることができます。

また、HTML以外にも、JSONやXMLなどの形式もパースすることができます。これらの形式はWeb開発においてよく使用されるため、パースの知識は非常に役立つものです。

## 関連リンク

- [html-conduit](https://hackage.haskell.org/package/html-conduit)
- [http-conduit](https://hackage.haskell.org/package/http-conduit)
- [select](https://hackage.haskell.org/package/select)