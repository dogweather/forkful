---
title:                "テキストファイルの読み込み"
html_title:           "Javascript: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ読むのか

Javascriptには、文字列や数値などのデータをファイルから読み込むための機能があります。これは、ウェブアプリケーションやゲームなど、様々なプログラムを作成する際に必要不可欠です。そのため、Javascriptを学ぶ上で、ファイルの読み込み方法を知っておくことは非常に重要です。

## 方法

ファイルを読み込むには、`FileReader()`という関数を使用します。以下の例では、`test.txt`というテキストファイルを読み込んで、その内容をコンソールに出力しています。

```Javascript
let reader = new FileReader(); // FileReader()関数を使用して新しいインスタンスを作成
reader.onload = function(e) {
  let fileContent = e.target.result; // 読み込んだファイルの内容を取得
  console.log(fileContent); // 内容をコンソールに出力
}
reader.readAsText('./test.txt'); // テキストファイルを読み込む
```

上記のコードを実行すると、`test.txt`の内容がコンソールに出力されます。

```
Hello, world!
```

## 深堀り

`FileReader()`は、ファイルの読み込みが完了するまで待機するため、非同期処理が必要です。また、`FileReader()`には、ファイルを読み込むためのさまざまなメソッドがあります。詳細については、[公式ドキュメント](https://developer.mozilla.org/ja/docs/Web/API/FileReader)を参照してください。

## 参考リンク

- [FileReader - MDN web docs](https://developer.mozilla.org/ja/docs/Web/API/FileReader)
- [Javascriptでファイルを読み込む方法 - Qiita](https://qiita.com/toduq/items/7690e1a4904e889d59ae)