---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:14:42.878136-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
REPL（Read-Eval-Print Loop）を使用すると、コードとリアルタイムで対話できます。入力を読み取り、評価し、結果を出力し、ループバックします。プログラマーは、スニペットをテストしたり、デバッグしたり、新しい言語をリアルタイムで学んだりするために使用します。

## 方法：
Goには組み込みのREPLは含まれていませんが、サードパーティのツールを使用できます。人気のあるツールの一つが `gore` です：

```go
// goreをインストールするには
$ go install github.com/motemen/gore/cmd/gore@latest

// goreを実行
$ gore
gore version 0.5.0  :help for help
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Hello, Go REPL!
nil
```

## 詳細解説
もともとLisp用に開発されたREPLは、PythonやRubyのような動的言語で一般的です。静的に型付けされたGoは、標準搭載されていません。`gore`の代替となるツールには、`go-pry`や`yaegi`があります。これらのツールはGoコードを解釈し、フルブローンのアプリをコンパイルすることなく、迅速にアイデアを探求し、検証することを可能にします。これらは特に初心者や、学習と実験が重視される教育的文脈で有用です。

## 参照
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
