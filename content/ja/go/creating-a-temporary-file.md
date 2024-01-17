---
title:                "一時ファイルの作成"
html_title:           "Go: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何？なぜ？

一時ファイルを作成するとは、一時的にデータを保持するためにプログラマーが作成する一時的なファイルのことです。 私たちプログラマーは、データを一時的に保存したり、バックアップしたり、データの削除を回避するために、多くの場合一時ファイルを作成します。

## 方法：

```Go
// 一時ファイルを作成して、内容を書き込む方法
file, err := ioutil.TempFile("", "sample")
if err != nil {
    panic(err)
}
defer os.Remove(file.Name())

// ファイルにデータを書き込む
_, err = file.WriteString("Hello world!")
if err != nil {
    panic(err)
}
```

```Go
// 既存のファイルを元に一時ファイルを作成する方法
input, err := ioutil.ReadFile("existing_file")
if err != nil {
    panic(err)
}
err = ioutil.WriteFile("new_temp_file", input, 0644)
if err != nil {
    panic(err)
}
```

## 深い掘り下げ：

一時ファイルが作成されるのは何十年も前からで、プログラマーにとって非常に便利な機能であると言えます。一時ファイルの代替として、メモリ内でデータを保持する方法や、別の名前でファイルを保存する方法もあります。一時ファイルは、ファイルシステムに直接影響を与えないため、データの削除を避けるのに便利です。

Go言語では、一時ファイルを作成する際に一意の名前を生成するためにランダムな文字列が使用されます。また、一時ファイルはプログラムが終了した際に自動的に削除されるため、開発者は明示的に削除する必要はありません。

## 関連情報：

- [ioutilのドキュメント](https://golang.org/pkg/io/ioutil/#TempFile)
- [一時ファイルの作成についてのブログ記事](https://dev.to/nitish_the_techie/temporary-files-in-go-3c4k)
- [一時ファイルの代替方法についてのスタックオーバーフローの質問](https://stackoverflow.com/questions/63913041/current-usage-for-temporary-files-in-go)