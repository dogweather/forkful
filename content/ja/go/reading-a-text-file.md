---
title:                "Go: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ読み込みファイルをするのか

テキストファイルを読み込むことは、プログラムで非常に重要なタスクです。テキストファイルには、様々なデータを格納することができ、それを読み取ることでプログラムの機能を大幅に拡張することができます。この記事では、Go言語でテキストファイルを読み込む方法を紹介します。

## 方法

まずは、ファイルを開くために`os`パッケージをインポートしましょう。

```Go
import "os"
```

次に、`os.Open()`関数を使用してファイルを開き、`os.File`オブジェクトを作成します。

```Go
file, err := os.Open("sample.txt")
if err != nil {
    panic(err)
}
defer file.Close()
```

`defer`キーワードを使用することで、`file.Close()`関数を忘れずに呼び出すことができます。

次に、`bufio`パッケージを使用してファイルを読み込みます。

```Go
scanner := bufio.NewScanner(file)
for scanner.Scan() {
    fmt.Println(scanner.Text())
}
if err := scanner.Err(); err != nil {
    panic(err)
}
```

このコードでは、`scanner.Scan()`関数がファイルの一行を読み込み、`fmt.Println()`関数によりその内容を表示しています。

## 深い掘り下げ

`bufio`パッケージには、ファイルの読み込みに関する便利な機能がたくさんあります。例えば、`scanner.Bytes()`関数を使用すると、ファイルの一行をバイト配列として取得することができます。また、`scanner.Split()`関数を使用すると、カスタムの区切り記号を指定することでファイルを分割し、より詳細な解析が可能になります。

## 他の参考記事

- [Goでファイルを読み込む方法](https://posts.career-zine.net/golang-file-io/)
- [Goでテキストファイルを操作する方法](https://www.javadrive.jp/go/file/index4.html)
- [osパッケージのドキュメント](https://golang.org/pkg/os/)
- [bufioパッケージのドキュメント](https://golang.org/pkg/bufio/)