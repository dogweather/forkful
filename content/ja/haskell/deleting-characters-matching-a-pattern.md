---
title:    "Haskell: パターンに一致する文字を削除する"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

あるパターンにマッチする文字を削除することについて、なぜ誰かがそれに関わることになるのかを説明します。

パターンがマッチする文字を削除することは非常に便利であり、データのクリーニングや処理を行う際に非常に有用です。例えば、特定の文字列から特定の文字を取り除く場合などに使用することができます。

## 試してみよう

下のコードブロックにHaskellコードの例とサンプルの出力を示します。

```Haskell
-- 文字列から"abc"を削除する例
deletePattern :: String -> String
deletePattern [] = [] -- 空の文字列の場合はそのまま返す
deletePattern (x:xs)
    | x == 'a' = deletePattern xs -- 'a' が見つかったら再帰で削除
    | x == 'b' = deletePattern xs
    | x == 'c' = deletePattern xs
    | otherwise = x:deletePattern xs -- マッチしない文字はそのまま追加

main = do
    putStrLn "文字列から'abc'を削除します。"
    let str = "abcdef"
    putStrLn $ "元の文字列: " ++ str
    putStrLn $ "パターンを削除した結果: " ++ deletePattern str

-- 出力:
-- 元の文字列: abcdef
-- パターンを削除した結果: def
```

## 深く掘り下げる

パターンマッチングや再帰などのHaskellの基本的な概念が理解されていることが、このような文字の削除に役立ちます。また、上記の例では単純な文字列だけを扱っていますが、リストやタプルなどの他のデータ型でも同様のアプローチが使えます。さらに、リスト内包表記や高階関数など、より洗練された方法でパターンにマッチする文字を削除することができます。

## 関連リンクを見る

[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - Haskellの基礎から応用までを学べるオンライン教材  
[Haskell Wiki](https://wiki.haskell.org/) - Haskellに関する情報が集まっているWikiwikiサイト  
[Real World Haskell](http://book.realworldhaskell.org/) - 実際のプロジェクトでHaskellを使う方法が学べるオンラインブック