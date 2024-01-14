---
title:    "C++: 文字列の連結"
keywords: ["C++"]
---

{{< edit_this_page >}}

# なぜ

文字列の連結に取り組む理由は、あなたのプログラムで文字列を動的に作成する必要があるからです。例えば、ユーザーからの入力を受け取り、それを他の文字列と組み合わせて出力する必要がある場合などです。

# 方法

文字列を連結する方法はいくつかありますが、今回はC++の標準ライブラリである`std::string`を使用して説明します。まず、`+`演算子を使用して文字列を簡単に連結することができます。

```C++
std::string first_name = "太郎";
std::string last_name = "山田";
std::string full_name = first_name + " " + last_name;
```

上記のコードでは、`first_name`と`last_name`の間にスペースを追加して、`full_name`という新しい文字列を作成しています。また、`+=`演算子を使用して、既存の文字列に新しい文字列を追加することもできます。

```C++
std::string message = "こんにちは、";
message += full_name;
message += "さん！";
```

上記の例では、`message`という文字列に`full_name`の値を追加し、最後に「さん！」という文字列を追加しています。

# 深堀り

文字列の連結を行う際には、パフォーマンスの観点から気をつける必要があります。一度に大量の文字列を連結する場合は、`std::string`を使用するよりも、`std::stringstream`を使用した方が効率的です。また、文字列を連結する際にはメモリ管理にも注意が必要です。大量の文字列を連結した後、不要な文字列を削除する必要がある場合は、`std::string`の`erase()`関数を使用してメモリを解放することができます。

# 参考リンク

- [文字列を連結する（C++ドキュメント）](https://docs.microsoft.com/ja-jp/cpp/standard-library/string-class?view=msvc-160#concatenating-strings)
- [C++で文字列操作をマスターする方法（Qiita）](https://qiita.com/tchnkmr/items/973a8436c2335f41f55c)