---
title:    "C++: 文字列の長さを見つける"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

こんにちは、C++プログラミングの皆さん。今日は、string（文字列）の長さを見つける方法についてお話ししたいと思います。

## Why
まずは、なぜ誰もが文字列の長さを見つける必要があるのでしょうか？文字列の長さを知ることで、例えばユーザーが入力した文字列が指定した長さを満たしているかどうかをチェックしたり、文字列の一部分を取り出す際に必要な長さを知ることができます。

## How To
それでは、実際にC++で文字列の長さを見つける方法を見ていきましょう。まずは以下のように、文字列の変数を宣言します。

```C++
string str = "Hello World";
```

そして、提供されている関数である`length()`を使います。この関数は、文字列の長さを返すので、簡単に使えます。

```C++
cout << "文字列の長さは：" << str.length() << endl;
```

このコードを実行すると、以下のような出力が得られます。

```C++
文字列の長さは：11
```

さらに、ユーザーから文字列を入力させてその長さを見つける場合には、`cin`を使って文字列を受け取り、`length()`関数を使って長さを取得することができます。

```C++
string input;
cout << "文字列を入力してください：";
cin >> input;
cout << "入力された文字列の長さは：" << input.length() << endl;
```

こちらも実行すると、以下のような結果が得られます。

```C++
文字列を入力してください：こんにちは
入力された文字列の長さは：5
```

簡単ですね！

## Deep Dive
さて、少し深く掘り下げてみましょう。`length()`関数の内部の仕組みを見てみると、実は`string`クラスのメンバー変数である`size`を返すだけのものになっています。なので、`str.size()`と書くこともできます。

また、文字列の長さを見つける方法は別にもあります。`string`クラスには他にも`size()`や`length()`以外にも、`size_type`という型を返す関数があります。これは、文字列の長さを表す型で、この値を使って文字列の長さを扱うことができます。例えば、以下のように書くことができます。

```C++
string str = "Hello World";
str.size_type len = str.size();
```

こうすることで、`str`の長さを表す`size_type`型の変数`len`を作ることができます。

以上が、簡単に文字列の長さを見つける方法についての説明でした。

## See Also
さらに深く学びたい方は、以下のリンクを参考にしてみてください。

- [string lengthのドキュメント](https://www.geeksforgeeks.org/length-function-in-cpp/)
- [string sizeのドキュメント](https://www.programiz.com/cpp-programming/string-size)
- [Data Structures and Algorithms - String Length](https://www.tutorialspoint.com/data_structures_algorithms/string_length.htm)