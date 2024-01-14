---
title:                "Go: 一時ファイルの作成"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

Go言語を学ぶために、一時ファイルの作成方法を学ぶ理由はたくさんあります。一時ファイルを作成することで、簡単にデータを保存することができます。また、一時ファイルの作成はデバッグ時に便利です。

## 作り方

一時ファイルの作成は、```Go ioutil.TempFile```関数を使用することで簡単に行うことができます。下記のコードは、一時ファイルを作成し、書き込み、読み込みを行う例です。

```Go
// 一時ファイルを作成する
file, err := ioutil.TempFile("", "temporary_file")
if err != nil {
    panic(err)
}
defer os.Remove(file.Name())
fmt.Println("一時ファイルが作成されました。")

// ファイルにデータを書き込む
data := []byte("これは一時ファイルです。")
_, err = file.Write(data)
if err != nil {
    panic(err)
}
fmt.Println("データをファイルに書き込みました。")

// ファイルからデータを読み込む
readData := make([]byte, 20)
_, err = file.Read(readData)
if err != nil {
    panic(err)
}
fmt.Println("ファイルから読み込んだデータは、", string(readData), "です。")
```

上記のコードを実行すると、下記のような結果が得られます。

```Go
一時ファイルが作成されました。
データをファイルに書き込みました。
ファイルから読み込んだデータは、これは一時ファイルです。です。
```

## ディープダイブ

```Go ioutil.TempFile```関数では、デフォルトで一時ファイルが「/tmp」ディレクトリに作成されますが、引数を指定することで任意のディレクトリに一時ファイルを作成することもできます。また、一時ファイルは自動的に削除されるため、開発中やテスト時に便利ですが、プログラムが終了するとファイルも削除されてしまいます。そのため、必要に応じてファイルを保持する必要があります。

## 参考リンク

[Go ioutil.TempFileドキュメント](https://golang.org/pkg/io/ioutil/#TempFile)