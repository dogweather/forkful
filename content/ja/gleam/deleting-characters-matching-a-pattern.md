---
title:                "パターンに一致する文字を削除する"
html_title:           "Gleam: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何&なんで？
「パターンに合致する文字を削除する」とは、文字列中の特定のパターンにマッチする文字を取り除くことを指します。プログラマーがこれを行う理由は、不要なデータを取り除いて処理を効率的にするためです。

## 使い方：
```Gleam
  // パターンに合致する文字を削除する例:
  let text = "Hello World!"
  let pattern = /[a-z]/g
  text |> String.replace(pattern, "") 
  // 出力: "H W!"
```

```Gleam
  // パターンに合致する文字を削除する例:
  let numbers = [1, 2, 3, 4, 5]
  let pattern = N.type({ type => 
    let isEven = fn(n) => (n % 2) == 0 
    let isOdd = fn(n) => !(isEven n)
    
    Ok if Type.equals(isEven(n)) == True
    then Error("Even numbers are not allowed!")
    else if Type.equals(isOdd(n)) == True 
    then Ok (Maybe.Just(n))
    else Error("Something went wrong!")
  })
  
  numbers 
    |> List.map(pattern)
    |> List.filter_map(fn(x) => x.),match 
    // 出力: [1, 3, 5]
```

## ディープダイブ：
パターンに合致する文字を削除する技術は、古くから存在します。他の言語では、正規表現や条件分岐を使い実現することもできますが、Gleamでは型システムを利用することでより型安全で堅牢な実装が可能になります。また、処理速度も向上するため、大規模なデータを扱う際にも有用です。

## 関連情報：
- [Gleam公式サイト](https://gleam.run/basics/pattern-matching)
- [Gleamの型システムについて：より安全なコードを書くためのヒント](https://www.red-gate.com/simple-talk/blogs/a-tasty-elixir-pattern-matching/)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)