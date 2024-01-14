---
title:    "C++: パターンに一致する文字の削除"
keywords: ["C++"]
---

{{< edit_this_page >}}

## なぜ

プログラマーとして、時にはある文字パターンにマッチする文字を削除する作業が必要になるかもしれません。例えば、文字列内の特定の単語や記号を除外する場合などです。

## 方法

まずは`string`クラスの`erase()`関数を使います。この関数は指定した位置から指定した数の文字を削除することができます。例えば、文字列から"Hello"という単語を削除したい場合、以下のように書くことができます。

```C++
string str = "Hello, world!";
str.erase(0,5); // "Hello"を削除
cout << str; // "Hello"を除外した文字列を出力する
```

また、単語にマッチするかどうかを判定するためには、`find()`関数を使うことができます。以下は"Hello"という単語が文字列中に存在するかどうかを判定する例です。

```C++
string str = "Hello, world!";
if (str.find("Hello") != string::npos) {
    // "Hello"が存在する場合、削除するコードをここに書きます
}
```

## ディープダイブ

文字を削除する作業はプログラミングにおいて非常によく使用される技術です。文字列操作を行う上で、様々な場面で必要になるため、上手く使いこなせるようになることは重要です。

ただ、注意が必要な点が1つあります。文字列は不変なオブジェクトであるため、`erase()`関数を使用すると元の文字列が変更されてしまうことに気をつけてください。もし、元の文字列を保持したい場合は、別の文字列変数にコピーしておく必要があります。

## それ以外にも

- `erase()`関数の仕様: https://en.cppreference.com/w/cpp/string/basic_string/erase
- `find()`関数の仕様: https://en.cppreference.com/w/cpp/string/basic_string/find
- 文字列操作に関する記事: https://www.programiz.com/cpp-programming/string