---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:29.276177-07:00
description: "Go\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u30C7\u30D0\u30C3\
  \u30AC\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3068\u306F\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306E\u72B6\u614B\u3092\u8ABF\u3079\u305F\u308A\u5909\u66F4\u3057\u305F\
  \u308A\u3057\u3066\u3001\u305D\u306E\u6319\u52D5\u3092\u7406\u89E3\u3057\u305F\u308A\
  \u554F\u984C\u3092\u8A3A\u65AD\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3001\
  \u30C4\u30FC\u30EB\u3084\u6A5F\u80FD\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u3092\
  \u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u52B9\
  \u7387\u7684\u306B\u30D0\u30B0\u3092\u898B\u3064\u3051\u3066\u4FEE\u6B63\u3057\u3001\
  \u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u6700\u9069\u5316\u3057\u3001\u30B3\
  \u30FC\u30C9\u306E\u6B63\u78BA\u6027\u3092\u78BA\u4FDD\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.394315-06:00'
model: gpt-4-0125-preview
summary: "Go\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u30C7\u30D0\u30C3\u30AC\
  \u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u306E\u72B6\u614B\u3092\u8ABF\u3079\u305F\u308A\u5909\u66F4\u3057\u305F\u308A\
  \u3057\u3066\u3001\u305D\u306E\u6319\u52D5\u3092\u7406\u89E3\u3057\u305F\u308A\u554F\
  \u984C\u3092\u8A3A\u65AD\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3001\u30C4\
  \u30FC\u30EB\u3084\u6A5F\u80FD\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u3092\u6307\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u52B9\u7387\
  \u7684\u306B\u30D0\u30B0\u3092\u898B\u3064\u3051\u3066\u4FEE\u6B63\u3057\u3001\u30D1\
  \u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u6700\u9069\u5316\u3057\u3001\u30B3\u30FC\
  \u30C9\u306E\u6B63\u78BA\u6027\u3092\u78BA\u4FDD\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u306E\u4F7F\u7528"
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
