---
title:                "テキストファイルを書く"
html_title:           "Go: テキストファイルを書く"
simple_title:         "テキストファイルを書く"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なに & なぜ？
テキストファイルを書くとは、プログラマーがコンピューターに情報を記録することです。プログラマーがこれを行う理由は、データの永続性を確保し、後で再利用することができるようにするためです。

## 方法：
```Go
// ファイルを作成し、開く
file, err := os.Create("sample.txt")
if err != nil {
    log.Fatal(err)
}
defer file.Close()

// 文字列を書き込む
str := "Hello, World!"
_, err = file.WriteString(str)
if err != nil {
    log.Fatal(err)
}
```

上記のコードは、"sample.txt"という名前のファイルを作成し、開き、"Hello, World!"という文字列を書き込みます。

## 深堀り：
(1) テキストファイルの歴史的背景：テキストファイルは、コンピューターに情報を永続的に記録するために使用されてきました。初期のコンピューターでは、命令やプログラムを保存するために使用されていましたが、現在では様々な用途に使用されています。

(2) 代替手段：テキストファイルの代わりに、データベースやクラウドストレージなどが使用されることもあります。しかし、データの永続性やポータビリティを考えると、テキストファイルの使用は依然として重要です。

(3) 実装詳細：Goでは、osパッケージを使用してファイルを作成し、io/ioutilパッケージを使用してファイルに書き込むことができます。ファイルの存在をチェックするために、os.Stat関数を使用することもできます。

## 関連情報：
- [Go 官公式ドキュメント](https://golang.org/pkg/)
- [Goでファイルを操作する方法](https://astaxie.gitbooks.io/build-web-application-with-golang/ja/04.5.html)