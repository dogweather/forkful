---
title:                "連想配列の使用"
aliases:
- /ja/go/using-associative-arrays/
date:                  2024-02-03T18:11:05.988553-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となく理由

連想配列は、Goではマップとして知られており、各ユニークキーが値にマップする場所にキー値ペアを保存できます。プログラマーは、効率的なデータの検索、修正、そして独自のキーを使って速くアクセスできる要素のコレクションを維持するためにマップを使用します。

## 使い方:

Goでマップを作成して初期化する方法はいくつかあります。始めるための基本的な例をここに示します:

```go
package main

import "fmt"

func main() {
    // マップの宣言と初期化
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // 出力: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

要素を追加または更新するには、以下のようにキーに値を割り当てます:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// 出力: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

キーで値にアクセスするのは簡単です:

```go
fmt.Println("The hex code for red is:", colors["red"])
// 出力: The hex code for red is: #FF0000
```

要素を削除するには、`delete`関数を使用します:

```go
delete(colors, "red")
fmt.Println(colors)
// 出力: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

マップを反復処理するには、forループを使用します:

```go
for color, hex := range colors {
    fmt.Printf("Key: %s Value: %s\n", color, hex)
}
```

Goのマップは順不同であることを覚えておいてください。反復の順序は保証されません。

## 深堀り

Goでは、マップはハッシュテーブルとして実装されています。マップの各エントリには2つの項目が含まれています：キーと値。エントリはキーをハッシュして保存されます。これにより、小さなデータセットの場合は定数時間の操作が可能であり、適切なハッシュ化が行われた場合の平均時間の複雑さはO(1)となりますが、多くのハッシュ衝突が発生する最悪の場合にはO(n)に劣化する可能性があります。

新しいGoプログラマーにとって重要な注記は、マップタイプが参照タイプであるということです。これは、関数にマップを渡すと、その関数内でマップに行われた変更が呼び出し元に対して可視であることを意味します。これは、例えば、構造体を関数に渡す場合と異なります。その場合、構造体はポインタによって渡されない限りコピーされます。

マップは連想配列を扱う場合には非常に多目的で効率的ですが、パフォーマンスが重要なアプリケーションでは、キーの分布が頻繁な衝突を引き起こす可能性がある場合、特に予測可能なパフォーマンス特性を持つデータ構造を使用することが有益かもしれません。

また、Go 1.9 以降で利用可能な`sync.Map`も検討する代替手段の一つです。これは、キーが一度書き込まれるが多くの回読み込まれる場合に使用されることを目的としており、これらのシナリオでの効率改善を提供します。しかし、一般的なGoアプリケーションにおいては、そのシンプルさと言語での直接サポートにより、通常のマップの使用が慣用的であり、推奨されるアプローチであることが多いです。
