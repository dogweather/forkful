---
title:                "デバッガの使用"
aliases:
- /ja/go/using-a-debugger/
date:                  2024-02-03T18:10:29.276177-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガの使用"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/using-a-debugger.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Goプログラミングにデバッガを使用することとは、プログラムの状態を調べたり変更したりして、その挙動を理解したり問題を診断したりするために、ツールや機能を利用することを指します。プログラマーは、効率的にバグを見つけて修正し、パフォーマンスを最適化し、コードの正確性を確保するためにこれを行います。

## 方法：

Goは`delve`と呼ばれる組み込みのデバッグ機能を提供しています。これはフル機能のデバッグツールで、Goプログラムをステップ実行し、プログラム変数を調査し、式を評価することができます。

始めるには、まず`delve`をインストールする必要があります。以下を実行することでインストールできます：

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

これで、簡単なGoプログラムをデバッグする準備が整いました。プログラム`main.go`を考えましょう：

```go
package main

import "fmt"

func main() {
    message := "Debugging in Go"
    fmt.Println(message)
}
```

このプログラムをデバッグを開始するには、プロジェクトのディレクトリでターミナルを開き、次を実行します：

```shell
dlv debug
```

このコマンドは、デバッグ体験を改善するために最適化を無効にしてプログラムをコンパイルし、それを起動してデバッガをアタッチします。

`delve`が実行されると、インタラクティブなデバッガシェルに入ります。いくつかの基本的なコマンドは以下の通りです：

- `break main.main` は`main`関数にブレークポイントを設定します。
- `continue` はプログラムの実行をブレークポイントがヒットするまで再開します。
- `print message` は`message`変数の値を出力します。
- `next` はプログラムの実行を次の行へ進めます。
- `quit` はデバッガを終了します。

ブレークポイントに到達して変数を出力した際の出力は、以下のようになります：

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Debugging in Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Debugging in Go"
```

これらのコマンドを使用して、プログラムをステップごとに進めながら状態を確認し、挙動を理解し、問題を特定することができます。

## 深堀り

Goの実行モデルとランタイムの性質により、GDB（GNU Debugger）などの従来のツールではなく`delve`がGoのデバッガとしての選択肢となっているのはそのためです。GDBは最初からGoランタイムを念頭に置いて設計されていなかったため、`delve`がGo開発者にとってより適した選択肢となります。`Delve`はGo専用に設計されており、Goルーチン、チャネル、その他のGo固有の構成要素のデバッグ体験をより直感的にすることができます。

さらに、`delve`はGoプログラムで働く際、GDBが提供する基本機能に加えて、実行中のプロセスへのアタッチ、条件付きブレークポイント、Goの並行性プリミティブを含むかもしれない複雑な式の評価など、幅広い機能をサポートしています。

`delve`が多くのGo開発者にとってのデフォルトのデバッガであるとはいえ、Goツールチェーンには、プロファイリング用の組み込み`pprof`ツールや、並行性の可視化用の`trace`ツールなど、より軽量なデバッグサポート形式も含まれています。これらのツールは、プログラムのパフォーマンス問題や並行性バグを診断するためのより速いまたはより高レベルの方法を提供することがあり、デバッグの文脈によっては、補完的またはより好ましい選択肢となることもあります。
