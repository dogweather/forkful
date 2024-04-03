---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:29.276177-07:00
description: "\u65B9\u6CD5\uFF1A Go\u306F`delve`\u3068\u547C\u3070\u308C\u308B\u7D44\
  \u307F\u8FBC\u307F\u306E\u30C7\u30D0\u30C3\u30B0\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\
  \u3066\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u30D5\u30EB\u6A5F\u80FD\u306E\u30C7\
  \u30D0\u30C3\u30B0\u30C4\u30FC\u30EB\u3067\u3001Go\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u3092\u30B9\u30C6\u30C3\u30D7\u5B9F\u884C\u3057\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u5909\u6570\u3092\u8ABF\u67FB\u3057\u3001\u5F0F\u3092\u8A55\u4FA1\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002 \u59CB\u3081\u308B\u306B\u306F\u3001\u307E\
  \u305A`delve`\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3059\u308B\u5FC5\u8981\u304C\
  \u3042\u308A\u307E\u3059\u3002\u4EE5\u4E0B\u3092\u5B9F\u884C\u3059\u308B\u3053\u3068\
  \u3067\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3067\u304D\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.394315-06:00'
model: gpt-4-0125-preview
summary: "Go\u306F`delve`\u3068\u547C\u3070\u308C\u308B\u7D44\u307F\u8FBC\u307F\u306E\
  \u30C7\u30D0\u30C3\u30B0\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\
  \u3002\u3053\u308C\u306F\u30D5\u30EB\u6A5F\u80FD\u306E\u30C7\u30D0\u30C3\u30B0\u30C4\
  \u30FC\u30EB\u3067\u3001Go\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u30B9\u30C6\u30C3\
  \u30D7\u5B9F\u884C\u3057\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5909\u6570\u3092\u8ABF\
  \u67FB\u3057\u3001\u5F0F\u3092\u8A55\u4FA1\u3059\u308B\u3053\u3068\u304C\u3067\u304D\
  \u307E\u3059."
title: "\u30C7\u30D0\u30C3\u30AC\u306E\u4F7F\u7528"
weight: 35
---

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
