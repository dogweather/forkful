---
title:                "Haskell: 正規表現の使用"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ正規表現を使うのか

正規表現はコンピューターやプログラミングにおいて、パターンマッチングや文字列検索、置換などの作業を行う際に非常に便利です。正規表現を使用することで、手作業で行うよりも素早く正確な結果を得ることができます。

## 正規表現の使い方

正規表現を使うには、まずはじめにGHCのパッケージマネージャーであるCabalをインストールします。次に、Haskellのコード内で正規表現を使うためにはregex-compatパッケージをインストールする必要があります。

```Haskell
cabal update
cabal install regex-compat
```

インストールが完了したら、以下のようにコード内で正規表現を使うことができます。

```Haskell
import Text.Regex

-- 文字列をマッチングさせるための正規表現パターンを定義
pattern :: Regex
pattern = mkRegex "hello[0-9]+"

-- マッチする文字列を取得し、リストとして返す関数
matchString :: String -> [String]
matchString str = getAllTextMatches (str =~ pattern)

main :: IO ()
main = do
  let result = matchString "hello123 world hello456"
  putStrLn $ show result -- ["hello123", "hello456"]
```

上記の例では、"hello"の後に数字が続く文字列を正規表現パターンとして定義し、そのパターンとマッチする部分をリストとして取得しています。

## 正規表現の詳細

正規表現を使う際には、パターンの中で特別な意味を持つ文字や文字クラスがあります。例えば、"?"や"*"などの特殊文字があり、これらはエスケープする必要があります。また、正規表現には"|"を使うことで複数のパターンをマッチングさせることもできます。

さらに、マッチング対象となる文字列をグループ分けすることで、後からそのグループを取得することもできます。例えば、"hello([0-9]+)world"のように定義することで、"hello"と"world"の間の数字だけを取得することができます。

正規表現の詳細については、以下のリンクを参考にしてください。

## 関連情報

- [Haskell正規表現チュートリアル](https://qiita.com/7shi/items/145f123961b073be3a6f)
- [正規表現の基礎知識](https://www.atmarkit.co.jp/ait/articles/2103/10/news015.html)
- [正規表現プログラミング](https://www.geocities.jp/m_hiroi/func/prog/re.html)

## 参考

- [Cabal](https://www.haskell.org/cabal/)
- [regex-compatパッケージのドキュメント](https://hackage.haskell.org/package/regex-compat-0.95.2/docs/Text-Regex-Compat.html)