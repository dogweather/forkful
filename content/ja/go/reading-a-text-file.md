---
title:                "テキストファイルの読み込み"
html_title:           "Go: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何？ なんで？
テキストファイルとは、テキスト（文字）で構成されたファイルのことです。プログラマーは、テキストファイルを読み込むことで、データを取得し、プログラムに使用することができます。

## やり方：
```Go
package main

import (
    "fmt"
    "os"
    "bufio"
)

func main() {
    // ファイルを開く
    file, err := os.Open("file.txt")
    
    if err != nil {
        fmt.Println("ファイルを開くことができませんでした。")
        os.Exit(1)
    }
    
    // ファイルを閉じる
    defer file.Close()
    
    // ファイルを１行ずつ読み込む
    scanner := bufio.NewScanner(file)
    
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }
    
    // エラーをチェックする
    if err := scanner.Err(); err != nil {
        fmt.Println("ファイルを読み込めませんでした。")
        os.Exit(1)
    }
}
```

出力：
```
This is a sample text file.
It is used to show how to read a text file in Go language.
```

## 詳細を深く掘り下げる：
テキストファイルを読み込むことは、コンピューターの歴史の中で非常に重要な役割を果たしてきました。これまで、テキストファイルの形式はさまざまなプログラミング言語で使用されてきました。また、テキストファイルを読み込む方法として、Go言語以外にも様々な方法があります。プログラマーは、自分のプロジェクトに最適な方法を選択することができます。

## 関連情報：
- [公式ドキュメント](https://golang.org/pkg/os/#Open)
- [ファイルの読み込みに関するチュートリアル](https://www.calhoun.io/reading-files-in-go/)
- [ファイル操作に関する記事](https://www.geeksforgeeks.org/reading-a-file-line-by-line-using-readline-module-in-python/)