---
title:                "パターンに一致する文字を削除する"
html_title:           "Haskell: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## それは何か？
「パターンにマッチする文字を削除する」とは、文字列中から特定のパターンに一致する文字を除外することです。プログラマーはこれを行うことで、文字列処理やデータクレンジングなどのタスクを効率的に実行することができます。

## 方法：
```Haskell
-- 文字列から数字のみを除外する関数
removeDigits :: String -> String
removeDigits str = [c | c <- str, not (elem c "0123456789")]

-- 使用例
removeDigits "abc123def456" --> "abcdef"
```

## 深堀り
1. 歴史的な文脈：文字列処理は、古くからプログラミングで使用されており、その中にはパターンに一致する文字を除外する必要がある場合があります。
2. 代替方法：他の言語やツールでも同様の機能を実現することができますが、Haskellでは高度なパターンマッチングとリスト内包表記の組み合わせにより、簡潔かつ効率的に文字列を加工することができます。
3. 実装の詳細：上記の例では、リスト内包表記を使用して文字列を処理していますが、実際にはリストを他の方法でフィルタリングすることも可能です。

## 関連情報
- [Haskellのリスト内包表記の使い方](https://qiita.com/satosystems/items/9194d5d0829f34896aad)
- [他の言語における文字列処理の方法](https://www.tutorialspoint.com/data_structures_algorithms/string_processes.htm)