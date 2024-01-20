---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## それは何 & なぜ?

テキストファイルを読むことは、文字データをコードから取得するプロセスを指します。これはプログラマーがデータの解析、ストレージの使用節約、または外部からの情報の取り込みを行うために行います。

## やり方:

```Go
// ファイルを開く
file, err := os.Open("textfile.txt")
if err != nil {
    log.Fatalf("Error opening file: %s", err)
}
defer file.Close()

// ファイルを読み込む
b, err := ioutil.ReadAll(file)
if err != nil {
    log.Fatalf("Error reading file: %s", err)
}
fmt.Println(string(b))
```

上記のコードは "textfile.txt"というテキストファイルを開き、その内容を表示します。

## ディープダイブ:

テキストファイルの読み込みは、プログラミングの初期から存在してきました。これは基本的なデータストレージとコミュニケーションの形式として広く使用されています。

代替の方法としては、バイナリファイルの読み取りやJSON、XMLなどの形式を解析することが挙げられます。だが、テキストファイルの読み取りはそのシンプルさから信頼性が高いとされています。

実装の詳細については、Go言語では `os` パッケージを使ってファイルへのアクセスを制御します。`ioutil` パッケージの `ReadAll` 関数はファイルをByteArrayに読み込んでいます。

## 関連情報:

1. [Go言語のパッケージドキュメンテーション](https://golang.org/pkg/)
2. [ioutil.ReadAllについての詳細](https://golang.org/pkg/io/ioutil/#ReadAll)