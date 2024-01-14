---
title:    "Haskell: デバッグ出力の印刷"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# なぜ
デバッグ出力を生成することは、コード内の問題を特定するために非常に便利であるため、プログラマーにとって非常に重要です。

# 方法
```Haskell
import Debug.Trace

-- デバッグ出力を生成する関数
printDebug :: Int -> Int -> String -> IO ()
printDebug a b c = trace ("aの値: " ++ show a ++ ", bの値: " ++ show b ++ ", cの値: " ++ c) $ print (a + b)
```
この例では、`printDebug`関数を使用して、変数`a`、`b`、`c`をデバッグ出力として表示しています。実行すると、以下のような結果が得られます。

```Haskell
> printDebug 1 2 "debug"
aの値: 1, bの値: 2, cの値: "debug"
3
```
このようにして、私たちは`a`、`b`、および`c`の値を確認することができます。

# 深堀り
デバッグ出力を生成する方法は、さまざまな方法があります。上記の例では、`Debug.Trace`モジュールを使用しましたが、他の方法もあります。例えば、`Data.List`モジュールの`intercalate`関数を使用することで、変数の値をより見やすい形式で出力することができます。

```Haskell
import Data.List

-- デバッグ出力を生成する関数
printDebug :: [Int] -> IO ()
printDebug xs = putStrLn $ "リストの要素: " ++ intercalate ", " (map show xs)

-- リストの要素が正しいことを確認する関数
checkList :: [Int] -> Bool
checkList [] = True
checkList (x:xs)
  | x > 10 = False
  | otherwise = checkList xs
```
ここでは、`printDebug`関数を使用して、リストの要素をデバッグ出力として表示しています。また、`checkList`関数を使用して、リストの要素が正しいかどうかをチェックしています。実行すると、以下のような結果が得られます。

```Haskell
> let myList = [1, 2, 3, 4, 5]
> printDebug myList
リストの要素: 1, 2, 3, 4, 5
> checkList myList
True
> let myList2 = [1, 20, 3, 4, 5]
> printDebug myList2
リストの要素: 1, 20, 3, 4, 5
> checkList myList2
False
```
デバッグ出力を生成することで、リストの要素の順番や値が正しいことを確認することができます。

# 関連記事
- [デバッグ出力を生成するためのさまざまな方法](https://haskellexplained.com/2018/01/19/haskell-debugging-tips/)
- [Haskellにおけるテストの基本](https://engineering.backtrace.io/testing-fundamentals-in-haskell/)

## 参考リンク
- [Haskell Wiki](https://wiki.haskell.org/)
- [Haskellで始める関数型プログラミング](https://book.mynavi.jp/manatee/detail/id=94235)