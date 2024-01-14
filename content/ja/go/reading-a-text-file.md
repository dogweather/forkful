---
title:                "Go: テキストファイルを読み込む"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

# なぜ

ファイルを読み込むことの重要性は何でしょうか？テキストファイルを読み取ることは、プログラマーにとって非常に役立ちます。例えば、データ処理や情報の抽出などに使用することができます。

# 使い方

ファイルを読み込むためには、Go言語の標準パッケージである「os」を使用します。以下のコード例を参考にしてみてください。

```Go
file, err := os.Open("sample.txt") // ファイルを開く
if err != nil {
    panic(err) // エラー発生時に処理を停止
}
defer file.Close() // 処理が終わったらファイルを閉じる

// 読み込んだデータをbyte配列に格納
data := make([]byte, 100)
count, err := file.Read(data) // ファイルを100バイト分読み込む
if err != nil {
    panic(err)
}
fmt.Printf("Read %d bytes: %s\n", count, data) // 読み込んだデータを出力

// 行単位でデータを読み込む例
scanner := bufio.NewScanner(file)
for scanner.Scan() {
    fmt.Println(scanner.Text())
}
```

実行結果：

```
Read 100 bytes: Hello World! This is a sample text file.
This is the second line.
This is the third line.
```

# 深堀り

ファイルを読み込む方法には、上記のように単純に一度にデータを読み込む方法や、行単位でデータを読み込む方法以外にも様々な方法があります。例えば、特定の文字列が含まれる行のみを読み込んだり、ファイル内のデータをパースして特定の形式で出力することもできます。また、ファイルを開いたまま内容を変更することもできます。詳細な使い方やコード例は、公式ドキュメントを参考にしてみてください。

# 関連情報を見る

- [Go言語の標準パッケージ「os」のドキュメント](https://golang.org/pkg/os/)
- [ファイルを読み込むための標準パッケージ「bufio」のドキュメント](https://golang.org/pkg/bufio/)
- [ファイル操作を行うためのパッケージ「ioutil」のドキュメント](https://golang.org/pkg/io/ioutil/)