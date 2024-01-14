---
title:                "C++: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ？

文字列の連結に取り組む理由はさまざまです。例えば、複数の変数を組み合わせて新しい文字列を作成する必要があったり、文字列を動的に変更する必要があったりする場合があります。また、データベースから取得した情報を人が読みやすい形式に変換する際にも文字列の連結が必要になることがあります。

## 方法

文字列の連結を行うには、C++で用意されている`+`演算子を利用します。例えば、2つの変数`string1`と`string2`を連結する場合、以下のように書きます。

```C++
string1 + string2
```

もしくは、`+=`演算子を使って変数の値を更新することもできます。

```C++
string1 += string2
```

また、文字列の連結には`concat()`関数を使うこともできます。`concat()`関数は複数の引数を受け取り、それらを連結した文字列を返します。例えば、

```C++
concat(string1, string2)
```

のように使うことができます。

文字列の連結が正しく行われているかどうかを確認するために、`cout`を使って結果をコンソールに出力することもできます。例えば、

```C++
cout << string1 + string2 << endl;
```

とすると、`string1`と`string2`が連結された結果が表示されます。

## 深堀り

文字列の連結を行う際には注意すべき点があります。文字列の長さが大きいほど連結にかかる処理時間も長くなり、メモリの消費量も増えます。そのため、大きな文字列を連結する場合は、`concat()`関数を使うよりも`+=`演算子を使った方が効率的です。また、文字列の連結を繰り返し行う場合は、毎回新しい文字列を作成して連結するよりも、最初から大きな文字列を作成してそこにデータを追加する方が良いでしょう。

## これらを参考にしてみてください

[3つの方法でC++で文字列の連結を行う方法](https://www.geeksforgeeks.org/concatenate-strings-in-cpp/)

[文字列の連結パフォーマンスの比較](https://programming.guide/string-concatenation-performance.html)

[文字列の操作を標準出力する方法](https://www.techiedelight.com/print-strings-characters-cpp/)