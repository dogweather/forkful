---
title:    "Bash: 一時ファイルを作成する"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ
一時ファイルを作る理由は、プログラミング中に重要なデータを一時的に保存するためです。これにより、プログラムがクラッシュしたときでもデータが失われなくなります。

## 方法
一時ファイルを作成するのは簡単です。まず、`mktemp`コマンドを使用して一時ファイルの名前を生成します。

```Bash
tempfile=$(mktemp)
```

次に、一時ファイルに書き込むデータを指定します。

```Bash
echo "This is a temporary file" > $tempfile
```

最後に、一時ファイルを使用して処理を行います。

```Bash
cat $tempfile
```

出力は以下のようになります。

```
This is a temporary file
```

## 深堀り
一時ファイルを作成するときは、一意の名前をつけることが重要です。そのために`mktemp`コマンドを使用します。また、一時ファイルはプログラムが終了すると自動的に削除されるため、クリーンアップの手間も省けます。

ただし、一時ファイルはプログラムが実行される間だけ存在するため、重要なデータを長期間保存するのには適していません。また、ファイルが保存される場所もランダムになるため、必要なファイルを見つけるのに苦労するかもしれません。

## 関連情報
[マスタリング Bash コマンドライン](https://www.amazon.co.jp/%E3%83%9E%E3%82%B9%E3%82%BF%E3%83%AA%E3%83%B3%E3%82%B0-Bash-%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E3%83%A9%E3%82%A4%E3%83%B3-%E3%83%86%E3%82%AD%E3%82%B9%E3%83%88-%E3%83%96%E3%83%BC%E3%83%A9%E3%83%BC/dp/4774193842/)
[Bash 言語プログラミングレシピ](https://www.amazon.co.jp/Bash%E8%A8%80%E8%AA%9E%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E3%83%AC%E3%82%B7%E3%83%94-%E9%BA%BB%E6%A0%84-%E9%83%8E/dp/4774170117/)
[超入門 Bash スクリプト基礎編](https://www.amazon.co.jp/%E8%B6%85%E5%85%A5%E9%96%80-Bash%E3%82%B9%E3%82%AF%E3%83%AA%E3%83%97%E3%83%88%E5%9F%BA%E7%A4%8E%E7%B7%A8-%E4%B8%83%E5%B4%8E-%E4%B8%80%E5%AE%89-ebook/dp/B01N2WJGE1/)